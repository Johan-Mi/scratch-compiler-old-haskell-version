{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module UID
  ( UID
  , UIDState
  , newID
  , idJSON
  , runUIDGenerator
  ) where

import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import qualified Data.Text as T
import JSON (JValue(..))

type UID = T.Text

newtype UIDState =
  UIDState Word

newID :: MonadState UIDState m => m UID
newID = do
  UIDState counter <- get
  put $ UIDState $ counter + 1
  pure $ T.append "id-" $ T.pack $ show counter

idJSON :: Maybe UID -> JValue
idJSON = maybe JNull JStr

runUIDGenerator :: Monad m => StateT UIDState m a -> m a
runUIDGenerator = flip evalStateT $ UIDState 0
