module Utils.Either
  ( maybeToRight
  , justFailWith
  ) where

maybeToRight :: e -> Maybe a -> Either e a
maybeToRight e = maybe (Left e) Right

justFailWith :: (a -> b) -> Maybe a -> Either b ()
justFailWith f = maybe (Right ()) (Left . f)
