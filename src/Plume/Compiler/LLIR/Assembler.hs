module Plume.Compiler.LLIR.Assembler where

import Plume.Compiler.LLIR.Syntax qualified as LLIR
import Plume.Compiler.LLIR.Free qualified as LLIR
import Plume.Compiler.Desugaring.Syntax qualified as Pre
import Plume.Compiler.ClosureConversion.Syntax qualified as Pre
import Control.Monad.IO
import GHC.IO qualified as IO
import Data.IntMap qualified as IntMap
import Data.Set qualified as Set
import Data.Map qualified as Map
import Data.List qualified as List
import Plume.Syntax.Common qualified as Cmm
import Plume.Syntax.Translation.Generics

{-# NOINLINE natives #-}
natives :: IORef (Set Text)
natives = IO.unsafePerformIO $ newIORef Set.empty

{-# NOINLINE nativeFunctionsHandler #-}
nativeFunctionsHandler :: IORef LLIR.Libraries
nativeFunctionsHandler = IO.unsafePerformIO $ newIORef Map.empty

{-# NOINLINE constantPool #-}
constantPool :: IORef (Map Cmm.Literal Int)
constantPool = IO.unsafePerformIO $ newIORef Map.empty

{-# NOINLINE globals #-}
globals :: IORef (Set Text)
globals = IO.unsafePerformIO $ newIORef Set.empty

fetchConstant :: Cmm.Literal -> IO Int
fetchConstant lit = do
  cnst <- readIORef constantPool
  case Map.lookup lit cnst of
    Just i -> pure i
    Nothing -> do
      modifyIORef' constantPool (Map.insert lit (Map.size cnst))
      pure (Map.size cnst)

class Assemble a where
  assemble :: a -> IOReader (Set Text) [LLIR.Segment]

instance Assemble a => Assemble [a] where
  assemble = fmap concat . mapM assemble

shouldNotBeLabel :: LLIR.Segment -> Bool
shouldNotBeLabel (LLIR.Function {}) = False
shouldNotBeLabel _ = True

extractFrom :: LLIR.Segment -> [LLIR.Instruction]
extractFrom (LLIR.Function {}) = error "Not implemented"
extractFrom (LLIR.Instruction instr) = [instr]

instance Assemble Pre.DesugaredProgram where
  assemble (Pre.DPFunction name args body) = do
    glbs <- readIORef globals
    ntvs <- readIORef natives
    let reserved = glbs <> ntvs
    
    let args' = Set.fromList args
        freed = LLIR.free reserved body
        env   = freed <> args'

    body' <- local (<> env) $ concat <$> mapM assemble body

    let localSpaceSize = Set.size env

    let body'' = filter shouldNotBeLabel body'
        finalBody = concatMap extractFrom body''

    let locals = List.nub $ args <> Set.toList freed
        localSpace = zip locals [0..]

    pure [LLIR.Function name args localSpaceSize localSpace finalBody]

  assemble (Pre.DPStatement stmt) = assemble stmt

  assemble (Pre.DPDeclaration name expr) = do
    expr' <- assemble expr
    pure (expr' <> LLIR.storeGlobal name)
  
  assemble (Pre.DPMutDeclaration name expr) = do
    expr' <- assemble expr
    pure (expr' <> [LLIR.Instruction LLIR.MakeMutable] <>  LLIR.storeGlobal name)

  assemble (Pre.DPMutUpdate update expr) = do
    update' <- assemble update
    expr' <- assemble expr
    pure (expr' <> update' <> [LLIR.Instruction LLIR.Update])

  assemble (Pre.DPNativeFunction fp name _ isStd) = do
    whenM (doesNativeAlreadyExist name) $ do
      error $ "Native function " <> show name <> " already exists"

    modifyIORef' natives (Set.insert name)
    nativesHandlers <- readIORef nativeFunctionsHandler

    nameConstantIdx <- liftIO $ fetchConstant (Cmm.LString name)
    let libIdx = case Map.lookup fp nativesHandlers of
          Just (LLIR.MkNativeLibrary { LLIR.nativeAddressLibrary = addr }) ->
            addr
          Nothing -> Map.size nativesHandlers

    let funLibIdx = case Map.lookup fp nativesHandlers of
          Just l -> Map.size l.nativeFunctions
          Nothing -> 0

    let newLibFunction = LLIR.NativeFunction nameConstantIdx funLibIdx libIdx
    modifyIORef' nativeFunctionsHandler (\m -> do
        case Map.lookup fp m of
          Just l -> Map.insert fp (l { LLIR.nativeFunctions = Map.insert name newLibFunction (l.nativeFunctions) }) m
          Nothing -> Map.insert 
            fp 
            (LLIR.MkNativeLibrary fp libIdx isStd (Map.singleton name newLibFunction)) m
      )
    pure []

instance Assemble Pre.DesugaredStatement where
  assemble (Pre.DSExpr expr) = assemble expr

  assemble (Pre.DSDeclaration name expr) = do
    expr' <- assemble expr
    pure (expr' <> LLIR.storeLocal name)
  
  assemble (Pre.DSMutDeclaration name expr) = do
    expr' <- assemble expr
    pure (expr' <> [LLIR.Instruction LLIR.MakeMutable] <> LLIR.storeLocal name)
  
  assemble (Pre.DSMutUpdate update expr) = do
    update' <- assemble update
    expr' <- assemble expr
    pure (expr' <> update' <> [LLIR.Instruction LLIR.Update])
  
  assemble (Pre.DSReturn expr) = do
    expr' <- assemble expr
    pure (expr' <> [LLIR.Instruction LLIR.Return])

instance Assemble Pre.DesugaredExpr where
  assemble (Pre.DEVar name) = do
    locals <- ask
    if name `Set.member` locals
      then pure [LLIR.Instruction (LLIR.LoadLocal name)]
      else do
        nats <- readIORef natives
        if name `Set.member` nats
          then pure [LLIR.Instruction (LLIR.LoadNative name)]
          else pure [LLIR.Instruction (LLIR.LoadGlobal name)]
  
  assemble (Pre.DEApplication name args) = do
    args' <- concat <$> mapM assemble args
    let argsLength = length args
    locals <- ask
    if name `Set.member` locals
      then pure $ args' ++ [LLIR.Instruction (LLIR.CallLocal name argsLength)]
      else do
        nats <- readIORef natives
        if name `Set.member` nats
          then pure $ args' 
                    ++ LLIR.instr (LLIR.LoadNative name) 
                    ++ LLIR.instr (LLIR.Call argsLength)
          else pure $ args' ++ [LLIR.Instruction (LLIR.CallGlobal name argsLength)]

  assemble (Pre.DELiteral lit) = do
    i <- liftIO $ fetchConstant lit
    pure [LLIR.Instruction (LLIR.LoadConstant i)]
  
  assemble (Pre.DEList es) = do
    es' <- concat <$> mapM assemble es
    pure (es' <> [LLIR.Instruction (LLIR.MakeList (length es))])

  assemble (Pre.DEIndex list (Pre.DELiteral (Cmm.LInt i))) = do
    list' <- assemble list
    pure (list' <> LLIR.instr (LLIR.ListGet (fromInteger i)))

  assemble (Pre.DEIndex list index) = do
    list' <- assemble list
    index' <- assemble index
    pure (list' <> index' <> [LLIR.Instruction LLIR.GetIndex])
  
  assemble (Pre.DEProperty expr index) = do
    expr' <- assemble expr
    pure (expr' <> [LLIR.Instruction (LLIR.ListGet index)])

  assemble (Pre.DEDictionary dict) = assemble (Pre.DEList (IntMap.elems dict))

  assemble (Pre.DEIf cond then' else') = do
    let doesThenReturn = doesReturn then'
        doesElseReturn = doesReturn else'

    cond' <- assemble cond
    then'' <- concatMapM assemble then'
    else'' <- concatMapM assemble else'

    let thenJumpAddr = length then'' + if doesThenReturn then 2 else 1
        thenBranch   = LLIR.instr (LLIR.JumpElseRel thenJumpAddr)

        elseJumpAddr = if doesElseReturn then length else'' else length else'' + 1
        elseBranch   = [LLIR.Instruction (LLIR.JumpRel elseJumpAddr) | not doesElseReturn]

    -- print else''

    -- print (cond' <> thenBranch <> then'' <> elseBranch <> else'')

    pure (cond' <> thenBranch <> then'' <> elseBranch <> else'')

  assemble (Pre.DETypeOf _) = error "TypeOf is not implemented"
  assemble (Pre.DEIsConstructor _ _) = error "IsConstructor is not implemented"
  
  assemble (Pre.DEEqualsTo e1 e2) = do
    e1' <- assemble e1
    e2' <- assemble e2
    pure (e1' <> e2' <> [LLIR.Instruction (LLIR.Compare LLIR.EqualTo)])
  
  assemble (Pre.DEAnd e1 e2) = do
    e1' <- assemble e1
    e2' <- assemble e2
    pure (e1' <> e2' <> [LLIR.Instruction (LLIR.Compare LLIR.AndCmp)])
  
  assemble Pre.DESpecial = pure [LLIR.Instruction LLIR.Special]

  assemble (Pre.DESlice e i) = do
    e' <- assemble e
    pure (e' <> [LLIR.Instruction (LLIR.Slice i)])
  
  assemble (Pre.DEGreaterThan e i) = do
    e' <- assemble e
    i' <- liftIO $ fetchConstant (Cmm.LInt $ toInteger i)
    pure (e' <> LLIR.loadConstant i' <> [LLIR.Instruction (LLIR.Compare LLIR.GreaterThan)])
  
  assemble (Pre.DEListLength e) = do
    e' <- assemble e
    pure (e' <> [LLIR.Instruction LLIR.ListLength])
  
  assemble (Pre.DEUnMut e) = do
    e' <- assemble e
    pure (e' <> [LLIR.Instruction LLIR.UnMut])

instance Assemble Pre.Update where
  assemble (Pre.UVariable name) = do
    locals <- ask
    if name `Set.member` locals
      then pure [LLIR.Instruction (LLIR.LoadLocal name)]
      else pure [LLIR.Instruction (LLIR.LoadGlobal name)]
  
  assemble (Pre.UProperty u i) = do
    u' <- assemble u
    pure (u' <> [LLIR.Instruction (LLIR.ListGet i)])

isReturn :: Pre.DesugaredStatement -> Bool
isReturn (Pre.DSReturn _) = True
isReturn _ = False

doesReturn :: [Pre.DesugaredStatement] -> Bool
doesReturn = any isReturn

runLLIRAssembler :: (Assemble a, LLIR.Free a, LLIR.Name a) => a -> IO LLIR.Program
runLLIRAssembler xs = do
  writeIORef globals (LLIR.free mempty xs)
  writeIORef constantPool Map.empty
  writeIORef natives Set.empty
  writeIORef nativeFunctionsHandler Map.empty

  xs' <- runReaderT (assemble xs) Set.empty

  nats <- readIORef nativeFunctionsHandler
  constants' <- Map.toList <$> readIORef constantPool
  let constants'' = IntMap.fromList (invertAList constants')
  pure (xs', nats, IntMap.elems constants'')

getNativesNames :: (LLIR.Name a, LLIR.Free a) => a -> Set Text
getNativesNames x = do
  let allNames = LLIR.getNames x
      funNames = LLIR.free mempty x

  helper allNames funNames
  where
    -- Get names of all native functions, meaning all names except
    -- functions, declarations and native functions (got from free)
    helper :: Set Text -> Set Text -> Set Text
    helper allNames funNames = allNames Set.\\ funNames

getNativeFunctions :: LLIR.Libraries -> Map Text LLIR.NativeFunction
getNativeFunctions = Map.unions . map LLIR.nativeFunctions . Map.elems

invertAList :: [(b, a)] -> [(a, b)]
invertAList = map (\(a, b) -> (b, a))

doesNativeAlreadyExist :: MonadIO m => Text -> m Bool
doesNativeAlreadyExist name = do
  nats <- readIORef natives
  pure (name `Set.member` nats)