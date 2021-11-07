{-# LANGUAGE OverloadedStrings #-}

module JSON
  ( JValue(..)
  , showJSON
  ) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import Data.List (intersperse)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8Builder)
import Text.Printf (printf)

data JValue
  = JNull
  | JBool Bool
  | JNum Int
  | JDec Double
  | JStr T.Text
  | JArr [JValue]
  | JObj [(T.Text, JValue)]

showJSON :: JValue -> L.ByteString
showJSON = B.toLazyByteString . go
  where
    fmtString str =
      B.charUtf8 '"' <>
      encodeUtf8Builder (T.concatMap escape str) <> B.charUtf8 '"'
    go JNull = "null"
    go (JBool True) = "true"
    go (JBool False) = "false"
    go (JNum num) = B.intDec num
    go (JDec dec) = B.doubleDec dec
    go (JStr str) = fmtString str
    go (JArr arr) =
      B.charUtf8 '[' <>
      mconcat (intersperse (B.charUtf8 ',') $ go <$> arr) <> B.charUtf8 ']'
    go (JObj obj) =
      B.charUtf8 '{' <>
      mconcat
        (intersperse (B.charUtf8 ',') $
         (\(key, val) -> fmtString key <> B.charUtf8 ':' <> go val) <$> obj) <>
      B.charUtf8 '}'

escape :: Char -> T.Text
escape '"' = "\\\""
escape '\\' = "\\\\"
escape '\b' = "\\b"
escape '\f' = "\\f"
escape '\n' = "\\n"
escape '\r' = "\\r"
escape '\t' = "\\t"
escape c
  | c < '\x20' = T.pack $ printf "\\u%04x" $ fromEnum c
  | otherwise = T.singleton c
