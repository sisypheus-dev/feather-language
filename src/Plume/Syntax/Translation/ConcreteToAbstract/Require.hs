{-# LANGUAGE LambdaCase #-}

module Plume.Syntax.Translation.ConcreteToAbstract.Require where

import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Concrete qualified as CST
import Plume.Syntax.Parser
import Plume.Syntax.Translation.Generics
import System.Directory
import System.FilePath

convertRequire
  :: Translator Error CST.Expression AST.Expression
convertRequire f (CST.ERequire modName) = do
  -- Retrieve the current working directory to use it as a base for the
  -- module path.
  cwd <- ask

  -- Creating the absolute module path by joining the current working
  -- directory with the module name.
  let strModName = toString modName
  let modPath = cwd </> strModName

  liftIO (doesFileExist modPath) >>= \case
    False -> do
      pos <- readIORef positionRef
      throwError $ case pos of
        Just p -> ModuleNotFound modName p
        Nothing -> NoPositionSaved
    True -> do
      -- Reading the content of the module file.
      content <- decodeUtf8 @Text <$> readFileBS strModName

      -- Creating the next module current directory by taking the directory
      -- part of the module path.
      let newCurrentDirectory = takeDirectory modPath

      -- Parsing the module file and converting it to an abstract syntax tree.
      -- We need to use the local function to change the current directory
      -- without globally changing it.
      -- Returning the generated AST as a spreadable AST (just a list of
      -- expressions represented as a single expression).
      local (const newCurrentDirectory) $
        liftIO (parsePlumeFile modPath content) >>= \case
          Left err -> throwError $ ParserError err
          Right cst ->
            sequenceMapM f cst >>= \case
              Left err -> throwError err
              Right ast -> bireturn . Spread $ flat ast
convertRequire _ _ = throwError $ CompilerError "Received invalid require expression"

sequenceMapM
  :: (Monad m, Traversable t, Monad f)
  => (a -> f (m a1))
  -> t a
  -> f (m (t a1))
sequenceMapM f = (sequence <$>) . mapM f
