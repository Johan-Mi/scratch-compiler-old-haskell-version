{-# LANGUAGE FlexibleContexts #-}

module Block.Env
  ( Env(..)
  , withNext
  , withParent
  , withProcArgs
  ) where

import Control.Monad.Reader (MonadReader, local)
import qualified Data.Text as T
import Lens.Micro (Lens', set)
import UID (UID)

data Env =
  Env
    { _envParent :: Maybe UID
    , _envNext :: Maybe UID
    , _envProcs :: [(T.Text, [(T.Text, UID)])]
    , _envProcArgs :: [T.Text]
    , _envLocalVars :: [(T.Text, (UID, T.Text))]
    , _envSpriteVars :: [(T.Text, (UID, T.Text))]
    , _envGlobalVars :: [(T.Text, (UID, T.Text))]
    , _envLocalLists :: [(T.Text, (UID, T.Text))]
    , _envSpriteLists :: [(T.Text, (UID, T.Text))]
    , _envGlobalLists :: [(T.Text, (UID, T.Text))]
    }

envParent :: Lens' Env (Maybe UID)
envParent f env = (\x -> env {_envParent = x}) <$> f (_envParent env)

envNext :: Lens' Env (Maybe UID)
envNext f env = (\x -> env {_envNext = x}) <$> f (_envNext env)

envProcArgs :: Lens' Env [T.Text]
envProcArgs f env = (\x -> env {_envProcArgs = x}) <$> f (_envProcArgs env)

withParent :: MonadReader Env m => Maybe UID -> m a -> m a
withParent = local . set envParent

withNext :: MonadReader Env m => Maybe UID -> m a -> m a
withNext = local . set envNext

withProcArgs :: MonadReader Env m => [T.Text] -> m a -> m a
withProcArgs = local . set envProcArgs
