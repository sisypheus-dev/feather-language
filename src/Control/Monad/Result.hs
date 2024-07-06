module Control.Monad.Result where

import Language.Plume.Frontend.Parser qualified as P
import Language.Plume.Syntax.Internal.Type qualified as Ty
import GHC.Show qualified as S
import qualified Data.Text as Text
import qualified GHC.IO as IO

fromEither :: a -> Either b a -> a
fromEither _ (Right a) = a
fromEither a _ = a

handle :: (MonadIO m, Show a) => Either a b -> (b -> m c) -> m c
handle (Right a) f = f a
handle (Left err) _ = liftIO $ print err >> exitFailure

data PlumeError
  = ParseError P.ParseError
  | CyclicModuleDependency FilePath
  | ModuleNotFound FilePath
  | VariableNotFound Text
  | TypeNotFound Text
  | TypeMismatch Ty.PlumeType Ty.PlumeType
  | EmptySwitch
  | FieldNotFound Text Ty.PlumeType
  | InconsistentArgument Text
  | ExtensionNotFound Text Ty.PlumeType
  | CompilerError Text

instance Show PlumeError where
  show (ParseError e) = P.errorBundlePretty e
  show (CyclicModuleDependency path)
    = "Cyclic module dependency detected in " <> show path
  show (ModuleNotFound path)
    = "Module " <> show path <> " not found"
  show (VariableNotFound name)
    = "Variable " <> show name <> " not found"
  show (TypeNotFound name)
    = "Type " <> show name <> " not found"
  show (TypeMismatch t1 t2)
    = S.show t1 <> " is not equivalent to " <> S.show t2
  show EmptySwitch = "Empty switch statement"
  show (FieldNotFound name ty)
    = "Field " <> show name <> " not found in " <> S.show ty
  show (InconsistentArgument name)
    = "Inconsistent argument " <> show name
  show (ExtensionNotFound name ty)
    = "Extension " <> show name <> " not found for type " <> S.show ty
  show (CompilerError msg) = "Compiler error: " <> show msg

showError :: P.ParseError -> String
showError = P.errorBundlePretty

compilerError :: HasCallStack => Text -> a
compilerError msg = do
  let err = "PLUME INTERNAL ERROR: " <> msg

  let cs = getCallStack callStack
      callstack = Text.unlines $ map (("    - " <>) . fromString . prettySrcLoc . snd) cs
      pCallstack =
        if null cs
          then ""
          else "\n  A bug occured in Plume compiler.\n  CallStack:\n" <> callstack

  IO.unsafePerformIO $ do
    putStrLn . toString $ err <> pCallstack
    exitFailure