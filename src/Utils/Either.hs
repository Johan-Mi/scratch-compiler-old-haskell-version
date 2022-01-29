module Utils.Either
  ( maybeToRight
  ) where

maybeToRight :: e -> Maybe a -> Either e a
maybeToRight e = maybe (Left e) Right
