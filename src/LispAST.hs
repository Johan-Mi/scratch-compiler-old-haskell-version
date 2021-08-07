module LispAST
  ( LispAST(..)
  ) where

import qualified Data.Text as T
import Text.Printf (printf)

data LispAST
  = LispNum Double
  | LispString T.Text
  | LispSym T.Text
  | LispNode LispAST [LispAST]
  | LispUnquote LispAST
  deriving (Eq)

instance Show LispAST where
  show (LispNum n) = show n
  show (LispString s) = printf "\"%s\"" s
  show (LispSym s) = T.unpack s
  show (LispNode fun args) = printf "(%s)" $ unwords $ show <$> (fun : args)
  show (LispUnquote ast) = ',' : show ast
