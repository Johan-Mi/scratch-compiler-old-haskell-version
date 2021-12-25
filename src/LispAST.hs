module LispAST
  ( LispAST(..)
  , asTheFunction
  , getSym
  , getStr
  , subTrees
  ) where

import qualified Data.Text as T
import Lens.Micro (Traversal')

data LispAST
  = LispNum Double
  | LispString T.Text
  | LispSym T.Text
  | LispNode LispAST [LispAST]
  | LispUnquote LispAST
  deriving (Eq, Show)

asTheFunction :: ([LispAST] -> a) -> T.Text -> LispAST -> Maybe a
asTheFunction f name (LispNode (LispSym str) args)
  | str == name = Just $ f args
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
