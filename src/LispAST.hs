module LispAST
  ( LispAST(..)
  ) where

import qualified Data.Text as T

data LispAST
  = LispNum Double
  | LispString T.Text
  | LispSym T.Text
  | LispNode LispAST [LispAST]
  | LispUnquote LispAST
  deriving (Eq, Show)
