module Utils.Maybe
  ( partitionMaybe
  ) where

import Data.Either (partitionEithers)

partitionMaybe :: (a -> Maybe b) -> [a] -> ([a], [b])
partitionMaybe f =
  partitionEithers .
  map
    (\a ->
       case f a of
         Just b -> Right b
         Nothing -> Left a)
