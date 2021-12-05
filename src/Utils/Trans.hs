module Utils.Trans
  ( hoistExcept
  , orThrow
  ) where

import Control.Monad.Except
  ( Except
  , ExceptT
  , MonadError
  , liftEither
  , runExcept
  , throwError
  )

hoistExcept :: Monad m => Except s a -> ExceptT s m a
hoistExcept = liftEither . runExcept

orThrow :: MonadError e m => e -> Maybe a -> m a
orThrow e = maybe (throwError e) pure
