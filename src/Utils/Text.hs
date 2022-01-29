{-# LANGUAGE OverloadedStrings #-}

module Utils.Text
  ( cropTextTo
  , arrayify
  ) where

import qualified Data.Text as T

cropTextTo :: Int -> T.Text -> T.Text
cropTextTo len text
  | T.length text <= len = text
  | otherwise = T.take (len - 1) text `T.snoc` '…'

arrayify :: [T.Text] -> T.Text
arrayify text = "[" <> T.intercalate "," text <> "]"
