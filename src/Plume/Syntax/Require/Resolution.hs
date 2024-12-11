{-# LANGUAGE LambdaCase, MultiWayIf #-}

module Plume.Syntax.Require.Resolution where

import Plume.Syntax.Require.Monad qualified as M
import System.FilePath
import Data.Map qualified as Map
import Data.SortedList qualified as SL
import Data.Set qualified as Set
import Data.List qualified as List
import Control.Monad.Parser
import Plume.Syntax.Parser.Requires qualified as P
import Plume.Syntax.Parser.Lexer qualified as L
import Plume.Syntax.Parser qualified as P
import Plume.Syntax.Concrete qualified as HLIR
import Plume.Syntax.Common.Annotation qualified as HLIR
import System.Directory
import System.Path.NameManip (guess_dotdot, absolute_path)
import Data.Maybe (fromJust)
import System.IO.Pretty
import Control.Monad.Except
import Text.Megaparsec.Pos
import Control.Monad.Exception (compilerError)

fromEither :: a -> Either b a -> a
fromEither _ (Right a) = a
fromEither a _ = a

-- | Absolutize a relative path to make it avaiable globally
absolutize :: String -> IO String
absolutize aPath
    | "~" `isPrefixOf` aPath = case aPath of
      (_:tail') -> do
        homePath <- getHomeDirectory
        return $ normalise $ addTrailingPathSeparator homePath 
                             ++ tail'
      _ -> return aPath
    | otherwise = do
        pathMaybeWithDots <- absolute_path aPath
        return $ fromJust $ guess_dotdot pathMaybeWithDots

isStandardPath :: MonadIO m => FilePath -> m Bool
isStandardPath ('s':'t':'d':':':_) = return True
isStandardPath path = do
  m <- readIORef M.moduleState
  case m.standardPath of
    Just p -> return $ p `isPrefixOf` path
    Nothing -> return False

getPath :: M.MonadResolution m => FilePath -> Maybe FilePath -> m FilePath
getPath modName ext = do
  m <- readIORef M.moduleState
  let cwd = m.currentDirectory
  let strModName = toString modName -<.> fromMaybe "plm" ext
  let isStd = "std:" `isPrefixOf` modName
  let isMod = "mod:" `isPrefixOf` modName

  modPath <- if 
        | isStd -> do
            let p = m.standardPath
            case p of
              Just p' -> return $ p' </> drop 4 strModName
              Nothing -> liftIO $ do
                ppFailure "PLUME_PATH variable is not set" 
                exitFailure
        | isMod -> do
            let p = m.modulePath
            case p of
              Just p' -> return $ p' </> "modules" </> drop 4 strModName
              Nothing -> liftIO $ do
                ppFailure "PPM_PATH variable is not set"
                exitFailure
        | otherwise -> return $ cwd </> strModName 


  res <- liftIO $ absolutize modPath

  let resWithoutExt = dropExtension res
  isDir <- liftIO (doesFileExist resWithoutExt) >>= \case
    False | isMod -> liftIO (doesDirectoryExist resWithoutExt)
    _ -> return False

  if isDir then return $ resWithoutExt </> "main.plm" else return res

resolvePath :: M.MonadResolution m => FilePath -> Bool -> m M.ModuleUnit
resolvePath fp isPub = do
  st <- readIORef M.moduleState
  let cwd = st.currentDirectory

  let moduleName = makeRelative st.initialPath fp

  path <- getPath fp Nothing

  exists <- liftIO $ doesFileExist path
  unless exists . liftIO $ do
    ppFailure ("File " <> fromString path <> " does not exist")
    exitFailure

  stack <- readIORef M.importStack
  if path `elem` stack
    then do
      let p1 = SourcePos path (mkPos 1) (mkPos 1)
      let p2 = SourcePos path (mkPos 1) (mkPos 1)

      throwError ("Circular dependency detected with " <> toString path, (p1, p2))
    else modifyIORef M.importStack (path :)

  case Map.lookup path st.resolved of
    Just m -> do
      modifyIORef M.importStack (List.drop 1)
      pure m { M.public = isPub }
    Nothing -> do
      let st' = st { M.currentDirectory = takeDirectory path }
      writeIORef M.moduleState st'
      content <- readFileBS path
      let contentAsText = decodeUtf8 content
      (paths, ops) <- loadModule path contentAsText

      ms <- mapM (\(p, p') -> resolvePath (toString p) p') paths
      
      let m = M.MkModuleUnit moduleName path isPub ms mempty mempty mempty ops
      
      ast <- liftIO $ P.parsePlumeFile path contentAsText ops

      modifyIORef' M.moduleState (\s -> s { M.currentDirectory = cwd })
      
      case ast of
        Left err -> liftIO $ do
          parseError err path contentAsText
          exitFailure

        Right (ast', ops') -> do
          m' <- foldlM checkForUndefined m ast'

          modifyIORef' M.moduleState (\s -> s { M.resolved = Map.insert path m' (M.resolved s) })
          modifyIORef' M.resultState (List.nub . (<> ast'))
          modifyIORef' M.importStack (List.drop 1)

          return m' { M.operators = ops' <> ops }

loadModule :: M.MonadResolution m => FilePath -> Text -> m ([(Text, Bool)], SL.SortedList L.CustomOperator)
loadModule path contentAsText = do
  imports <- liftIO $ parse P.getPaths path contentAsText

  case imports of
    Left e -> liftIO $ do
      parseError e path contentAsText
      exitFailure

    Right modules -> do
      isStd <- isStandardPath path
      let prelude = "std:prelude"
      let modules' = if not isStd then (prelude, Nothing, False) : modules else modules
      
      operators <- liftIO $ parse P.parseOperators path contentAsText

      let operators' = fromEither mempty operators

      pure (map (\(path', _, pub) -> (path', pub)) modules', operators')

checkModule :: MonadIO m => (Maybe FilePath, Maybe FilePath) -> FilePath -> m [HLIR.Expression]
checkModule (env, mod') fp = do
  modifyIORef' M.moduleState $ \s -> s {
    M.initialPath = fp
  , M.currentDirectory = takeDirectory fp
  , M.standardPath = env
  , M.modulePath = mod'
  , M.boundArgs = ["#property", "#deref", "if", "if_else", "JS_BACKEND", "NATIVE_BACKEND", "?"]
  }

  let fp' = takeFileName fp

  res <-  runExceptT (resolvePath fp' True)

  case res of
    Left (err, p) -> liftIO $ do
      printErrorFromString Nothing (err, Nothing, p) "while resolving modules"
      exitFailure
    Right _ -> readIORef M.resultState

checkForUndefined :: M.MonadResolution m => M.ModuleUnit -> HLIR.Expression -> m M.ModuleUnit
checkForUndefined m (HLIR.EDeclaration _ name e1 e2) = do
  let ann = name.annotationName
  let var = (ann.identifier, ann.isMacro)

  let m' = m { M.variables = Set.insert var (M.variables m) }

  m'' <- checkForUndefined m' e1
  maybe (pure m'') (checkForUndefined m'') e2
checkForUndefined m (HLIR.EVariableDeclare _ name _) = do
  let m' = m { M.variables = Set.insert (name, False) (M.variables m) }
  pure m'
checkForUndefined m (HLIR.EType ann tcs) = do
  let m' = m { 
    M.types = Set.insert ann.annotationName.identifier (M.types m),
    M.variables = Set.union (Set.fromList $ map getFromTC tcs) (M.variables m)
  }
  pure m'
checkForUndefined m (HLIR.ETypeAlias ann _) = do
  let m' = m { M.types = Set.insert ann.annotationName.identifier (M.types m) }
  pure m'
checkForUndefined m (HLIR.EInterface ann _ annots _) = do
  let defs = Set.fromList $ map interpretAnnot annots
  let m' = m { 
    M.classes = Set.insert ann.annotationName.identifier (M.classes m),
    M.variables = Set.union defs (M.variables m) 
  }
  pure m'
checkForUndefined m (HLIR.EBlock es) = foldlM checkForUndefined m es
checkForUndefined m (HLIR.EProperty _ e) = checkForUndefined m e
checkForUndefined m (HLIR.EPublic e) = checkForUndefined m e
checkForUndefined m (HLIR.EApplication f xs) = do
  m' <- checkForUndefined m f
  foldlM checkForUndefined m' xs
checkForUndefined m (HLIR.EConditionBranch e1 e2 e3) = do
  m' <- checkForUndefined m e1
  m'' <- checkForUndefined m' e2
  maybe (pure m'') (checkForUndefined m'') e3
checkForUndefined m (HLIR.EList es) = foldlM checkForUndefined m es
checkForUndefined m (HLIR.ESwitch e cases) = do
  m' <- checkForUndefined m e
  foldlM (\m'' (pat, e') -> do
    let vars = getVariables pat
    old <- readIORef M.moduleState
    modifyIORef' M.moduleState $ \s -> s { M.boundArgs = s.boundArgs <> vars }
    res <- checkForUndefined m'' e'
    modifyIORef' M.moduleState $ \s -> s { M.boundArgs = old.boundArgs }
    pure res) m' cases
checkForUndefined m (HLIR.EClosure args _ body _) = do
  let args' = map interpretAnnot args
  old <- readIORef M.moduleState
  modifyIORef' M.moduleState $ \s -> s { M.boundArgs = s.boundArgs <> map fst args' }
  m' <- checkForUndefined m body
  modifyIORef' M.moduleState $ \s -> s { M.boundArgs = old.boundArgs }
  pure m'
checkForUndefined m (HLIR.EUnMut e) = checkForUndefined m e
checkForUndefined m (HLIR.EReturn e) = checkForUndefined m e
checkForUndefined m (HLIR.EVariable name _) = do
  let var@(n, _) = interpretIdentifier name
  bound <- readIORef M.moduleState

  pos <- M.fetchPosition
  if isVariableDefined 0 var m || n `elem` bound.boundArgs
    then pure m
    else throwError ("Variable " <> toString n <> " is not defined", pos)
checkForUndefined m (HLIR.ELiteral _) = pure m
checkForUndefined m (HLIR.ELocated e p) = M.pushPosition p *> checkForUndefined m e <* M.popPosition
checkForUndefined m (HLIR.EMutUpdate n e1 e2) = do
  let var@(n', _) = interpretAnnot n
  bound <- readIORef M.moduleState
  pos <- M.fetchPosition
  
  if isVariableDefined 0 var m || n' `elem` bound.boundArgs
    then do
      m' <- checkForUndefined m e1
      maybe (pure m') (checkForUndefined m') e2
    else throwError ("Variable " <> toString n' <> " is not defined", pos)
checkForUndefined m (HLIR.ERequire _) = pure m
checkForUndefined m (HLIR.ETypeExtension _ ann _ exts) = do
  let (var, _) = interpretAnnot ann

  unless (isClassDefined 0 var m) $ do
    pos <- M.fetchPosition
    throwError ("Class " <> toString var <> " is not defined", pos)

  let exts' = map toExpr exts
  
  foldlM checkForUndefined m exts'
checkForUndefined m (HLIR.ENativeFunction _ name _ _ _ _) = do
  pure m { M.variables = Set.insert (name, False) (M.variables m) }
checkForUndefined m (HLIR.EAwait e) = checkForUndefined m e
checkForUndefined m (HLIR.EWhile e1 e2) = do
  m' <- checkForUndefined m e1
  checkForUndefined m' e2
checkForUndefined m (HLIR.EInstanceDeclare _ name _) = do
  unless (isClassDefined 0 name m) $ do
    pos <- M.fetchPosition
    throwError ("Class " <> toString name <> " is not defined", pos)

  pure m
checkForUndefined m (HLIR.ELetMatch p e) = do
  let vars = getVariables p
  old <- readIORef M.moduleState
  modifyIORef' M.moduleState $ \s -> s { M.boundArgs = s.boundArgs <> vars }
  void $ checkForUndefined m e
  modifyIORef' M.moduleState $ \s -> s { M.boundArgs = old.boundArgs }
  pure m { M.variables = Set.union (Set.fromList (map (, False) vars)) (M.variables m) }
checkForUndefined m (HLIR.EMonadicBind var e) = do
  let (n, _) = interpretIdentifier var
  void $ checkForUndefined m e
  pure m { M.variables = Set.insert (n, False) (M.variables m) }
checkForUndefined m (HLIR.EDirectExtension _ ann exts) = do
  let (var, _) = interpretAnnot ann

  let exts' = map toExpr exts
  
  foldlM checkForUndefined (m { M.variables = Set.insert (var, False) m.variables }) exts'
checkForUndefined _ _ = compilerError "Unsupported expression"

toExpr :: HLIR.ExtensionMember -> HLIR.Expression
toExpr (HLIR.ExtDeclaration gens annot e) = HLIR.EDeclaration gens annot e Nothing

getFromTC :: HLIR.TypeConstructor t -> (Text, Bool)
getFromTC (HLIR.TConstructor name _) = (name, False)
getFromTC (HLIR.TVariable name) = (name, False)

interpretAnnot :: HLIR.Annotation t -> (Text, Bool)
interpretAnnot (HLIR.Annotation n _ _) = (n.identifier, n.isMacro)

interpretIdentifier :: HLIR.Identifier -> (Text, Bool)
interpretIdentifier (HLIR.MkIdentifier n m) = (n, m)

type Depth = Int

isVariableDefined :: Depth -> (Text, HLIR.IsMacro) -> M.ModuleUnit -> Bool
isVariableDefined depth v@(var, isM) m =
  Set.member (var, isM) m.variables
    || any (isVariableDefined (depth + 1) v) getPublicImports
 where
  getPublicImports :: [M.ModuleUnit]
  getPublicImports = filter (\m' -> m'.public || depth == 0) m.imports

isClassDefined :: Depth -> Text -> M.ModuleUnit -> Bool
isClassDefined depth c m =
  Set.member c m.classes
    || any (isClassDefined (depth + 1) c) getPublicImports
 where
  getPublicImports :: [M.ModuleUnit]
  getPublicImports = filter (\m' -> m'.public || depth == 0) m.imports

getVariables :: HLIR.Pattern -> [Text]
getVariables (HLIR.PVariable n _) = [n]
getVariables (HLIR.PConstructor _ ps) = concatMap getVariables ps
getVariables (HLIR.PList _ ps p) = concatMap getVariables ps <> maybe [] getVariables p
getVariables (HLIR.PSlice n _) = [n]
getVariables (HLIR.PSpecialVar _ _) = []
getVariables (HLIR.PLiteral _) = []
getVariables (HLIR.PWildcard _) = []