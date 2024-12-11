{-# LANGUAGE DefaultSignatures #-}

module Control.Monad.Exception where

import Control.Monad.Parser
import Prettyprinter.Render.Terminal.Internal
import Text.Megaparsec
import System.IO.Pretty (ppFailure, anBold, anCol, anItalic)
import GHC.IO (unsafePerformIO)
import System.Info (os, compilerVersion, arch)
import Data.Text qualified as Text
import Data.Version (showVersion)
import Prettyprinter (Pretty (pretty), layoutPretty, defaultLayoutOptions, (<+>))

class Throwable a where
  showError :: (HasCallStack) => a -> Text
  default showError :: (Pretty a) => a -> Text
  showError e =
    renderStrict $
      layoutPretty defaultLayoutOptions $
        pretty e

instance Throwable Text where
  showError err =
    renderStrict $
      layoutPretty defaultLayoutOptions $
        anBold (anCol Red "[error]") <+> pretty err

instance Throwable ParsingError where
  showError = toText . errorBundlePretty

class IOThrowable a where
  showErrorIO :: a -> IO b

instance IOThrowable Text where
  showErrorIO e = ppFailure e >> exitFailure
  

compilerError :: HasCallStack => Text -> a
compilerError e = unsafePerformIO $ do
  let cs = getCallStack callStack
  let callstack = Text.unlines $ map (("    - " <>) . fromString . prettySrcLoc . snd) cs
  let pCallstack = if null cs then "" else anCol Black "\n  CallStack:\n" <> pretty callstack
  let ver = fromString $ showVersion compilerVersion
  let errMsg = "\n\n"
        <> pretty e 
        <> anCol Black "\n\n  OS: " <> pretty os 
        <> anCol Black "\n  GHC: " <> ver 
        <> anCol Black "\n  Arch: " <> fromString arch 
        <> pCallstack 
        <> anItalic "\n\nPlease report this error to the Plume team by submitting an issue on the GitHub repository."

  putDoc (anBold (anCol Red "[error]") <> ": PLUME INTERNAL BUG" <+> errMsg)
  putStrLn ""
  exitFailure

instance IOThrowable String where
  showErrorIO e = ppFailure (toText e) >> exitFailure

-- catchIO is an utility function that aims at catching function that may
-- throw an error during execution and automatically print the error and
-- exiting the program with a failure status code.
-- If no error is thrown, the function will continue its execution by
-- applying the provided function to the right result.
with
  :: (MonadIO m, IOThrowable err)
  => m (Either err a)
  -> (a -> m b)
  -> m b
with m f = do
  x <- m
  case x of
    Left e -> liftIO $ showErrorIO e
    Right r -> f r

infix 1 `with`
