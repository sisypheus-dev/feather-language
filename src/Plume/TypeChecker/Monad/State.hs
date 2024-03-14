{-# LANGUAGE AllowAmbiguousTypes #-}

module Plume.TypeChecker.Monad.State where

import Data.Map qualified as Map
import GHC.IO
import GHC.Records
import Plume.Syntax.Concrete (Position)
import Plume.TypeChecker.Constraints.Definition
import Plume.TypeChecker.Monad.Type
import Plume.TypeChecker.Monad.Type.Scheme

type Environment = Map Text Scheme

data CheckerState = CheckerState
  { tvarCounter :: Int
  , variables :: Environment
  , types :: Environment
  , returnType :: PlumeType
  , constraints :: [TypeConstraint]
  , position :: Maybe Position
  , generics :: Map Text Int
  }

checkerST :: IORef CheckerState
{-# NOINLINE checkerST #-}
checkerST = unsafePerformIO $ newIORef emptyState

emptyState :: CheckerState
emptyState =
  CheckerState
    { tvarCounter = 0
    , variables = Map.empty
    , types = Map.empty
    , returnType = TUnit
    , constraints = []
    , position = Nothing
    , generics = Map.empty
    }

search
  :: forall l v m
   . (HasField l CheckerState (Map Text v), MonadIO m)
  => Text
  -> m (Maybe v)
search k = do
  v <- readIORef checkerST
  return $ Map.lookup k (getField @l v)