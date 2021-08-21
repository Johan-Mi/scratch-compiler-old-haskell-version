module Utils.Functor
  ( flap
  ) where

flap :: Functor f => f (a -> b) -> a -> f b
flap f a = ($ a) <$> f
