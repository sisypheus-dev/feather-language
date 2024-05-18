{-# LANGUAGE LambdaCase #-}

module Plume.Syntax.Translation.ConcreteToAbstract where

import Control.Monad.Exception
import Data.Map qualified as Map
import Data.List qualified as List
import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Common qualified as Common
import Plume.Syntax.Concrete qualified as CST
import Plume.Syntax.Translation.ConcreteToAbstract.MacroResolver
import Plume.Syntax.Translation.ConcreteToAbstract.Operations
import Plume.Syntax.Translation.ConcreteToAbstract.Require
import Plume.Syntax.Translation.ConcreteToAbstract.UFCS
import Plume.Syntax.Translation.Generics
import System.Directory
import System.FilePath
import qualified Data.Text as T

-- | The shared library extension for all platforms
{-# INLINE sharedLibExt #-}
sharedLibExt :: String
sharedLibExt = "plmc"

-- | Interpret spreadable by compiling a spreadable expression into
-- | a single expression
interpretSpreadable
  :: Spreadable [AST.Expression] AST.Expression 
  -> AST.Expression
interpretSpreadable (Single e) = e
interpretSpreadable (Spread [e]) = e
interpretSpreadable (Spread es) = AST.EBlock es
interpretSpreadable Empty = AST.EBlock []

-- | Spanning a property expression means to split the expression
-- | according to the UFCS rules that states:
-- | 
-- | x.property(arg) <=> property(x, arg)
-- | 
-- | So spanning such an expression results in obtaining the possibly
-- | nested property in a callee.
spanProperty
  :: CST.Expression 
  -> Maybe CST.Expression
spanProperty (CST.ELocated e _) = spanProperty e
spanProperty (CST.EProperty e p) = Just (CST.EProperty e p)
spanProperty _ = Nothing

-- | Main translation from concrete syntax to abstract syntax
-- | This transformation pass performs some syntactic sugar reductions
-- | such as UFCS introduction, macro expansion, import resolving,
-- | operator resolving, etc.
concreteToAbstract
  :: CST.Expression
  -> TranslatorReader Error AST.Expression
concreteToAbstract (CST.EVariable n) = transRet . Right $ AST.EVariable n
concreteToAbstract (CST.ELiteral l) = transRet . Right $ AST.ELiteral l
concreteToAbstract e@(CST.EBinary {}) = convertOperation concreteToAbstract e
concreteToAbstract e@(CST.EPrefix {}) = convertOperation concreteToAbstract e
concreteToAbstract e@(CST.EPostfix {}) = convertOperation concreteToAbstract e
concreteToAbstract m@(CST.EMacro {}) =
  convertMacro concreteToAbstract m
concreteToAbstract m@(CST.EMacroFunction {}) =
  convertMacro concreteToAbstract m
concreteToAbstract m@(CST.EMacroVariable _) =
  convertMacro concreteToAbstract m
concreteToAbstract m@(CST.EMacroApplication {}) =
  convertMacro concreteToAbstract m
concreteToAbstract (CST.EApplication e args)
  | Just e' <- spanProperty e = do
      convertUFCS concreteToAbstract (CST.EApplication e' args)
  | otherwise = do
      e' <- shouldBeAlone <$> concreteToAbstract e
      es' <- fmap flat . sequence <$> mapM concreteToAbstract args
      transRet $ AST.EApplication <$> e' <*> es'
concreteToAbstract (CST.EDeclaration g isMut ann e me) = do
  ann' <- mapM (mapM transformType) ann
  -- Declaration and body value cannot be spread elements, so we need to
  -- check if they are alone and unwrap them if they are.
  e' <- shouldBeAlone <$> concreteToAbstract e
  me' <- mapM shouldBeAlone <$> maybeM concreteToAbstract me
  transRet $ AST.EDeclaration isMut g ann' <$> e' <*> me'
concreteToAbstract (CST.EUnMut e) = do
  -- Unmut can be a spread element, so we need to check if it is and
  -- then combine the expressions into a single expression by wrapping
  -- them into a block.
  e' <- fmap interpretSpreadable <$> concreteToAbstract e
  transRet $ AST.EUnMut <$> e'
concreteToAbstract (CST.EConditionBranch e1 e2 e3) = do
  -- A condition should be a single expression
  e1' <- shouldBeAlone <$> concreteToAbstract e1

  -- But the branches can be spread elements, so we need to check if they
  -- are, and then combine them into a single expression by wrapping them
  -- into a block.
  e2' <- fmap interpretSpreadable <$> concreteToAbstract e2
  e3' <-
    fmap (fmap interpretSpreadable) . sequence <$> maybeM concreteToAbstract e3
  transRet $ AST.EConditionBranch <$> e1' <*> e2' <*> e3'
concreteToAbstract (CST.EClosure anns t e) = do
  anns' <- mapM (mapM (\(t', b) -> do
    case t' of
      Just t'' -> do
        t''' <- transformType t''
        return (Just t''', b)
      Nothing -> return (Nothing, b)
    )) anns
  t' <- mapM transformType t
  -- Same method as described for condition branches
  e' <- fmap interpretSpreadable <$> concreteToAbstract e
  transRet $ AST.EClosure anns' t' <$> e'
concreteToAbstract (CST.EBlock es) = do
  -- Blocks can be composed of spread elements, so we need to flatten
  -- the list of expressions into a single expression.
  es' <-
    fmap flat . sequence <$> do
      oldMacroSt <- readIORef macroState
      res <- mapM concreteToAbstract es
      writeIORef macroState oldMacroSt
      return res
  transRet $ AST.EBlock <$> es'
concreteToAbstract r@(CST.ERequire _) = do
  isImport <- asks snd
  if isImport
    then convertRequire concreteToAbstract r
    else bireturn Empty
concreteToAbstract (CST.ELocated e p) = do
  old <- readIORef positionRef
  writeIORef positionRef (Just p)

  res <-
    concreteToAbstract e `with` \case
      Single e' -> bireturn (Single (AST.ELocated e' p))
      Spread es -> bireturn (Spread es)
      Empty -> bireturn Empty

  writeIORef positionRef old
  return res
concreteToAbstract (CST.ESwitch e ps) = do
  -- Same method as described for condition branches
  e' <- shouldBeAlone <$> concreteToAbstract e
  ps' <-
    mapM sequence
      <$> mapM
        (\(p, body) -> (p,) . fmap interpretSpreadable <$> concreteToAbstract body)
        ps
  transRet $ AST.ESwitch <$> e' <*> ps'
concreteToAbstract (CST.EProperty i e) = do
  e' <- shouldBeAlone <$> concreteToAbstract e
  transRet $ AST.EApplication (AST.EVariable i) . (: []) <$> e'
concreteToAbstract (CST.EReturn e) = do
  -- Return can be a spread element, so we need to check if it is and
  -- then combine the expressions into a single expression by wrapping
  -- them into a block.
  e' <- fmap interpretSpreadable <$> concreteToAbstract e
  transRet $ AST.EReturn <$> e'
concreteToAbstract (CST.EListIndex e i) = do
  e' <- shouldBeAlone <$> concreteToAbstract e
  i' <- shouldBeAlone <$> concreteToAbstract i
  transRet $ AST.EApplication (AST.EVariable "get_index") <$> sequence [e', i']
concreteToAbstract (CST.ETypeExtension g ann var ems) = do
  ann' <- mapM (mapM transformType) ann
  ems' <-
    fmap flat . sequence <$> mapM concreteToAbstractExtensionMember ems
  transRet $ AST.ETypeExtension g ann' var <$> ems'
concreteToAbstract (CST.ENativeFunction fp n gens t) = do
  t' <- transformType t
  -- Native function resolution is kind the same as require resolution
  -- except we do not parse everything. But we need to resolve the path 
  -- absolutely to make it work everywhere on the system.
  let strModName = fromString $ toString fp -<.> sharedLibExt
  let (isStd, path) = if "std:" `T.isPrefixOf` fp then (True, T.drop 4 strModName) else (False, strModName)
  transRet . Right $ AST.ENativeFunction path n gens t' isStd
concreteToAbstract (CST.EList es) = do
  -- Lists can be composed of spread elements, so we need to flatten
  -- the list of expressions into a single expression.
  es' <-
    fmap flat . sequence <$> mapM concreteToAbstract es
  transRet $ AST.EList <$> es'
concreteToAbstract (CST.EType ann ts) = do
  ts' <- mapM transformTyCons ts
  bireturn . Single $ AST.EType ann ts'
concreteToAbstract (CST.EInterface ann gs ms) = do
  ann' <- mapM (mapM transformType) ann
  ms' <- mapM (mapM transformSch) ms
  bireturn . Single $ AST.EInterface ann' gs ms'
concreteToAbstract (CST.ETypeAlias ann t) = do
  let gens = map Common.getGenericName ann.annotationValue
  t' <- liftIO $ transformType t
  
  modifyIORef' typeAliases (Map.insert ann.annotationName (gens, t'))

  bireturn Empty

transformSch :: MonadIO m => Common.PlumeScheme -> m Common.PlumeScheme
transformSch (Common.MkScheme gens t) = do
  t' <- transformType t
  return $ Common.MkScheme gens t'

transformTyCons :: MonadIO m => CST.TypeConstructor Common.PlumeType -> m (CST.TypeConstructor Common.PlumeType)
transformTyCons (CST.TConstructor n ts) = do
  ts' <- mapM transformType ts
  return $ CST.TConstructor n ts'
transformTyCons (CST.TVariable n) = return $ CST.TVariable n

transformType :: MonadIO m => Common.PlumeType -> m Common.PlumeType
transformType (Common.TApp (Common.TId n) xs) = do
  xs' <- mapM transformType xs
  m <- readIORef typeAliases
  case Map.lookup n m of
    Just (gens, t) -> return $ substituteType t (zip gens xs')
    Nothing -> return $ Common.TApp (Common.TId n) xs'
transformType (Common.TApp t xs) = do
  t' <- transformType t
  xs' <- mapM transformType xs
  return $ Common.TApp t' xs'
transformType (Common.TId n) = do
  m <- readIORef typeAliases
  case Map.lookup n m of
    Just (_, t) -> return t
    Nothing -> return $ Common.TId n

substituteType :: Common.PlumeType -> [(Text, Common.PlumeType)] -> Common.PlumeType
substituteType (Common.TApp (Common.TId n) xs) subs = do
  let xs' = map (`substituteType` subs) xs
  case List.lookup n subs of
    Just t -> t
    Nothing -> Common.TApp (Common.TId n) xs'
substituteType (Common.TApp t xs) subs = do
  let xs' = map (`substituteType` subs) xs
  Common.TApp (substituteType t subs) xs'
substituteType (Common.TId n) subs =
  case List.lookup n subs of
    Just t -> t
    Nothing -> Common.TId n


-- | Translate a concrete extension member to an abstract extension member
concreteToAbstractExtensionMember
  :: CST.ExtensionMember Common.PlumeType
  -> TranslatorReader Error (AST.ExtensionMember Common.PlumeType)
concreteToAbstractExtensionMember (CST.ExtDeclaration g ann e) = do
  ann' <- mapM (mapM transformType) ann
  e' <- shouldBeAlone <$> concreteToAbstract e
  return $ Single . AST.ExtDeclaration g ann' <$> e'

-- | Entry translation function runner that redirects the translation
runConcreteToAbstract
  :: Maybe FilePath
  -> FilePath
  -> [(Text, Maybe CST.Position)]
  -> FilePath
  -> IO (Either Error [AST.Expression])
runConcreteToAbstract std dir paths fp = do
  -- Writing the standard library path to the IORef to keep it
  -- without needing to refetch it every time
  writeIORef stdPath std

  -- Getting the current working directory as a starting point
  -- for the reader monad
  cwd <- (</> dir) <$> getCurrentDirectory

  flip runReaderT (cwd, False) $ do
    -- Translating the imports to get the expressions that are
    -- imported from the modules
    exprs <- translateImports paths cwd concreteToAbstract

    -- Fetching again more generally the current directory and
    -- reading the content of the file
    newCWD <- liftIO getCurrentDirectory
    content <- liftIO $ decodeUtf8 @Text <$> readFileBS fp
    
    -- Parsing the main module file
    res <- parseFile (fp, content) newCWD

    -- Finally converting the main parsed file to abstract syntax
    local (const (newCWD, False)) $
      case res of
        Left err -> throwError' err
        Right cst -> sequenceMapM concreteToAbstract cst >>= \case
          Left err -> throwError' err
          Right ast -> bireturn $ exprs <> flat ast
