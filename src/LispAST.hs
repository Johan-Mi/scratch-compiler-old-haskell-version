module LispAST
  ( LispAST(..)
  , isTheFunction
  , getSym
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

isTheFunction :: T.Text -> LispAST -> Bool
isTheFunction func (LispNode (LispSym str) _) = str == func
isTheFunction _ _ = False

getSym :: LispAST -> Maybe T.Text
getSym (LispSym sym) = Just sym
getSym _ = Nothing
