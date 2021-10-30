module LispAST
  ( LispAST(..)
  , isTheFunction
  , asTheFunction
  , getSym
  , getStr
  , subTrees
  ) where

import Control.Monad (guard)
import Data.Functor (($>))
import qualified Data.Text as T
import Lens.Micro (Traversal')
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

asTheFunction :: ([LispAST] -> a) -> T.Text -> LispAST -> Maybe a
asTheFunction f name (LispNode (LispSym str) args) =
  guard (str == name) $> f args
asTheFunction _ _ _ = Nothing

getSym :: LispAST -> Maybe T.Text
getSym (LispSym sym) = Just sym
getSym _ = Nothing

getStr :: LispAST -> Maybe T.Text
getStr (LispString str) = Just str
getStr _ = Nothing

subTrees :: Traversal' LispAST LispAST
subTrees _ ast@(LispNum _) = pure ast
subTrees _ ast@(LispString _) = pure ast
subTrees _ ast@(LispSym _) = pure ast
subTrees f (LispNode x xs) = LispNode <$> f x <*> traverse f xs
subTrees f (LispUnquote x) = LispUnquote <$> f x
