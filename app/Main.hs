{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Exception
import Control.Monad.Parser
import Data.Text.IO hiding (putStr, writeFile)
import Plume.Compiler.ClosureConversion.Conversion
import Plume.Compiler.Desugaring.Desugar
import Plume.Compiler.TypeErasure.EraseType
import Plume.Syntax.Parser.Modules.ParseImports
import Plume.Syntax.Require.Resolution
import Plume.Syntax.Translation.ConcreteToAbstract
import Plume.Compiler.Javascript.Translate
import Plume.TypeChecker.Checker
import Plume.Syntax.Blocks
import Plume.Syntax.MatchRemoval
import System.Directory
import System.FilePath
import System.IO.Pretty
import CLI
import Prelude hiding (putStrLn, readFile)
import Plume.Syntax.ArgumentDeducer (deduceArgument)
#if defined(mingw32_HOST_OS)
import System.IO (hPutStrLn, hSetEncoding, stdout, utf8)
import System.Win32.Console (getConsoleOutputCP, setConsoleOutputCP)

setEncoding :: IO a -> IO a
setEncoding a = do
  cp <- getConsoleOutputCP
  setConsoleOutputCP 65001
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetEncoding stdin utf8

  res <- a

  setConsoleOutputCP cp

  pure res

#else
setEncoding :: IO a -> IO a
setEncoding = id
#endif

main :: IO ()
main = setEncoding $ do
  MkOptions file_input file_output remove_prelude <- parseOptions
  let output = fromMaybe file_input file_output

  env <- lookupEnv "PLUME_PATH"
  mod' <- lookupEnv "PPM_PATH"

  let file = file_input -<.> "plm"

  doesFileExist file >>= \case
    False -> do
      ppFailure ("File " <> fromString file <> " does not exist")
      exitFailure
    True -> pure ()

  cwd <- getCurrentDirectory >>= canonicalizePath
  let dir = cwd </> takeDirectory file

  content <- readFile file

  paths <- fromEither [] <$> parse getPaths file content
  let paths' = case env of
        Just _ | not remove_prelude -> do
          let preludeName = "prelude.plm"
          ("std:" <> preludeName, Nothing) : paths
        _ -> paths

  ppBuilding "Parsing file and dependencies..."

  void $ checkModule (env, mod') file

  runConcreteToAbstract env dir paths' file `with` \ast -> do
    let ast' = map deduceArgument ast
    let ast'' = manageLetMatches ast'
    let ast''' = concatMap (removeUselessBlocks (False, True)) ast''

    -- mapM_ print ast'''
    ppBuilding "Typechecking..."
    runSynthesize ast''' `with` \tlir -> do
      ppBuilding "Compiling and optimizing..."
      erased <- erase tlir
      runClosureConversion erased `with` \closed -> do
        desugared <- desugar closed

        let js   = runTranslateJS desugared
            code = createMainJSApp js

        let outputPath = output -<.> "js"
        writeFileText outputPath code
        ppSuccess ("Javascript code written to " <> fromString outputPath)
