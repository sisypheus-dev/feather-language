{-# LANGUAGE LambdaCase #-}

module Plume.Syntax.Translation.ConcreteToAbstract where

import Control.Monad.Exception
import Data.Map qualified as Map
import Data.List qualified as List
import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Common qualified as Common
import Plume.Syntax.Concrete qualified as CST
import Plume.Syntax.Translation.ConcreteToAbstract.Require
import Plume.Syntax.Translation.Generics
import System.Directory
import System.FilePath
import GHC.IO hiding (liftIO)
import qualified Data.Text as T

-- | The shared library extension for all platforms
{-# INLINE sharedLibExt #-}
sharedLibExt :: Text -> String
sharedLibExt "js" = "js"
sharedLibExt _ = "plmc"

{-# NOINLINE initialCWD #-}
initialCWD :: IORef FilePath
initialCWD = unsafePerformIO $ newIORef ""

-- | Interpret spreadable by compiling a spreadable expression into
-- | a single expression
interpretSpreadable
  :: Spreadable [AST.Expression] AST.Expression 
  -> AST.Expression
interpretSpreadable (Single e) = e
interpretSpreadable (Spread [e]) = e
interpretSpreadable (Spread es) = AST.EBlock es
interpretSpreadable Empty = AST.EBlock []

-- | Main translation from concrete syntax to abstract syntax
-- | This transformation pass performs some syntactic sugar reductions
-- | such as UFCS introduction, macro expansion, import resolving,
-- | operator resolving, etc.
concreteToAbstract
  :: CST.Expression
  -> TranslatorReader Error AST.Expression
concreteToAbstract (CST.ELiteral l) = transRet . Right $ AST.ELiteral l
concreteToAbstract (CST.EPublic e) = concreteToAbstract e
concreteToAbstract (CST.EVariable n t) = transRet . Right $ AST.EVariable n t
concreteToAbstract (CST.EApplication (CST.EProperty p e) args) = do
  e' <- shouldBeAlone <$> concreteToAbstract e
  args' <- fmap flat . sequence <$> mapM concreteToAbstract args
  let args'' = (:) <$> e' <*> args'

  transRet $ AST.EApplication (CST.EVariable (Common.fromText p) Nothing) <$> args''
concreteToAbstract (CST.EApplication e args) = do
  e' <- shouldBeAlone <$> concreteToAbstract e
  es' <- fmap flat . sequence <$> mapM concreteToAbstract args
  transRet $ AST.EApplication <$> e' <*> es'
concreteToAbstract (CST.EDeclaration g ann e me) = do
  ann' <- mapM (mapM transformType) ann
  -- Declaration and body value cannot be spread elements, so we need to
  -- check if they are alone and unwrap them if they are.
  e' <- shouldBeAlone <$> concreteToAbstract e
  me' <- mapM shouldBeAlone <$> maybeM concreteToAbstract me
  transRet $ AST.EDeclaration g ann' <$> e' <*> me'
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
  e3' <- sequence <$> case e3 of
    Just e3' -> 
      Just . fmap interpretSpreadable <$> concreteToAbstract e3'
    Nothing -> pure Nothing
  transRet $ AST.EConditionBranch <$> e1' <*> e2' <*> e3'
concreteToAbstract (CST.EClosure anns t e isA) = do
  anns' <- mapM (\(Common.Annotation name ty mut) -> do
    ty' <- mapM transformType ty
    return $ Common.Annotation name ty' mut
    ) anns
  t' <- mapM transformType t
  -- Same method as described for condition branches
  e' <- fmap interpretSpreadable <$> concreteToAbstract e
  transRet $ AST.EClosure anns' t' <$> e' <*> pure isA
concreteToAbstract (CST.EBlock es) = do
  -- Blocks can be composed of spread elements, so we need to flatten
  -- the list of expressions into a single expression.
  es' <-
    fmap flat . sequence <$> do
      res <- mapM concreteToAbstract es
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
concreteToAbstract (CST.EReturn e) = do
  -- Return can be a spread element, so we need to check if it is and
  -- then combine the expressions into a single expression by wrapping
  -- them into a block.
  e' <- fmap interpretSpreadable <$> concreteToAbstract e
  transRet $ AST.EReturn <$> e'
concreteToAbstract (CST.ETypeExtension g ann var ems) = do
  ann' <- mapM (mapM transformType) ann
  ems' <-
    fmap flat . sequence <$> mapM concreteToAbstractExtensionMember ems
  transRet $ AST.ETypeExtension g ann' var <$> ems'
concreteToAbstract (CST.ENativeFunction fp n gens t _) = do
  sc <- readIORef mode

  t' <- transformType t
  dir <- asks fst
  initialDir <- liftIO $ readIORef initialCWD

  let basePath = makeRelative initialDir dir

  -- Native function resolution is kind the same as require resolution
  -- except we do not parse everything.
  let strModName = fromString $ toString fp -<.> "js"
  let (ty, path)
        | "std:" `T.isPrefixOf` fp = 
            (Just "standard" :: Maybe Text, T.drop 4 strModName)
        | "mod:" `T.isPrefixOf` fp = 
            (Just "module", T.drop 4 strModName)
        | otherwise = 
            (Nothing, fromString $ basePath </> toString fp -<.> "js")

  let isPathPrefix :: FilePath -> FilePath -> Bool
      isPathPrefix p1 p2 = normalise p1 `List.isPrefixOf` normalise p2

  pMod <- liftIO $ readIORef modulePath
  sc' <- case pMod of
    Just p | (p </> "modules") `isPathPrefix` toString path -> return (Just "module")
    _ -> return sc

  newPath <- case sc' of
    Just "standard" -> do 
      p <- liftIO $ readIORef stdPath
      case p of
        Just p' -> do
          let diff = filepathDifference p' (toString path)
          return (fromString diff)
        Nothing -> return path
    Just "module" -> do
      p <- liftIO $ readIORef modulePath
      case p of
        Just p' -> do
          let diff = filepathDifference (p' </> "modules") (toString path)
          return (fromString diff)
        Nothing -> return path
    _ -> return path

  transRet . Right $ AST.ENativeFunction newPath n gens t' (sc' <|> ty)
concreteToAbstract (CST.EList es) = do
  -- Lists can be composed of spread elements, so we need to flatten
  -- the list of expressions into a single expression.
  es' <-
    fmap flat . sequence <$> mapM concreteToAbstract es
  transRet $ AST.EList <$> es'
concreteToAbstract (CST.EType ann ts) = do
  ts' <- mapM transformTyCons ts
  bireturn . Single $ AST.EType ann ts'
concreteToAbstract (CST.EInterface ann gs ms d) = do
  ann' <- mapM (mapM transformType) ann
  ms' <- mapM (mapM transformSch) ms
  bireturn . Single $ AST.EInterface ann' gs ms' d
concreteToAbstract (CST.ETypeAlias ann t) = do
  let gens = ann.annotationValue
  let name = ann.annotationName.identifier
  t' <- liftIO $ transformType t
  
  modifyIORef' typeAliases (Map.insert name (gens, t'))

  bireturn Empty
concreteToAbstract (CST.EVariableDeclare gens n t) = do
  t' <- mapM transformType t
  bireturn . Single $ AST.EVariableDeclare gens n t'
concreteToAbstract (CST.EAwait e) = do
  e' <- shouldBeAlone <$> concreteToAbstract e
  transRet $ AST.EApplication (AST.EVariable "wait" Nothing) . (: []) <$> e'
concreteToAbstract (CST.EWhile e1 e2) = do
  e1' <- shouldBeAlone <$> concreteToAbstract e1
  e2' <- shouldBeAlone <$> concreteToAbstract e2
  transRet $ AST.EWhile <$> e1' <*> e2'
concreteToAbstract (CST.EInstanceDeclare gens n t) = do
  t' <- mapM transformType t
  bireturn . Single $ AST.EInstanceDeclare gens n t'
concreteToAbstract (CST.ELetMatch p e) = do
  e' <- shouldBeAlone <$> concreteToAbstract e
  transRet $ AST.ELetMatch p <$> e'
concreteToAbstract (CST.EDirectExtension g t ems) = do
  ems' <-
    fmap flat . sequence <$> mapM concreteToAbstractExtensionMember ems
  transRet $ AST.EDirectExtension g t <$> ems'
concreteToAbstract (CST.EMonadicBind n e) = do
  e' <- shouldBeAlone <$> concreteToAbstract e
  transRet $ AST.EMonadicBind n <$> e'
concreteToAbstract _ = throwError' (CompilerError "Unsupported expression")

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
  :: CST.ExtensionMember
  -> TranslatorReader Error AST.ExtensionMember
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
  mod' <- lookupEnv "PPM_PATH"
  -- Writing the standard library path to the IORef to keep it
  -- without needing to refetch it every time
  writeIORef stdPath std
  writeIORef modulePath mod'
  initialDir <- absolutize dir
  writeIORef initialCWD initialDir

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
