{-# LANGUAGE FlexibleContexts #-}

module Block.Env
  ( Env(..)
  , withNext
  , withParent
  , withProcArgs
  ) where

import Control.Monad.Reader (MonadReader, local)
import qualified Data.Text as T
import UID (UID)

data Env =
  Env
    { envParent :: Maybe UID
    , envNext :: Maybe UID
    , envProcs :: [(T.Text, [(T.Text, UID)])]
    , envProcArgs :: [T.Text]
    , envLocalVars :: [(T.Text, (UID, T.Text))]
    , envSpriteVars :: [(T.Text, (UID, T.Text))]
    , envGlobalVars :: [(T.Text, (UID, T.Text))]
    , envLocalLists :: [(T.Text, (UID, T.Text))]
    , envSpriteLists :: [(T.Text, (UID, T.Text))]
    , envGlobalLists :: [(T.Text, (UID, T.Text))]
    }

withParent :: MonadReader Env m => Maybe UID -> m a -> m a
withParent parent = local $ \env -> env {envParent = parent}

withNext :: MonadReader Env m => Maybe UID -> m a -> m a
withNext next = local $ \env -> env {envNext = next}

withProcArgs :: MonadReader Env m => [T.Text] -> m a -> m a
withProcArgs args = local $ \env -> env {envProcArgs = args}
