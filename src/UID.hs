{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module UID
  ( UID
  , UIDState
  , newID
  , prependID
  , idJSON
  , runUIDGenerator
  ) where

import Control.Monad.State (MonadState, StateT, evalStateT, get, modify, put)
import Data.Bifunctor (first)
import qualified Data.Text as T
import JSON (JValue(..))

type UID = T.Text

type UIDState = ([UID], Word)

newID :: MonadState UIDState m => m UID
newID = do
  (prepends, counter) <- get
  case prepends of
    [] -> do
      put ([], counter + 1)
      return $ T.append "id-" $ T.pack $ show counter
    (x:xs) -> do
      put (xs, counter)
      return x

prependID :: MonadState UIDState m => UID -> m ()
prependID = modify . first . (:)

idJSON :: Maybe UID -> JValue
idJSON = maybe JNull JStr

runUIDGenerator :: Monad m => StateT UIDState m a -> m a
runUIDGenerator = flip evalStateT ([], 0)
