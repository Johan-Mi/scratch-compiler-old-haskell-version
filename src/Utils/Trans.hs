module Utils.Trans
  ( hoistExcept
  ) where

import Control.Monad.Except (Except, ExceptT, liftEither, runExcept)

hoistExcept :: Monad m => Except s a -> ExceptT s m a
hoistExcept = liftEither . runExcept
