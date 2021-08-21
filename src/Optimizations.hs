{-# LANGUAGE OverloadedStrings #-}

module Optimizations
  ( exprOptimizations
  ) where

import Mid.Expr (Expr(..), Value(..), getLit, toBool)
import Utils.Maybe (partitionMaybe)

exprOptimizations :: [Expr -> Maybe Expr]
exprOptimizations = [notNot, deMorgan, constNot, constAnd, constOr]

-- Remove double `not`s
notNot :: Expr -> Maybe Expr
notNot (FuncCall "not" [FuncCall "not" [expr]]) =
  Just $ FuncCall "boolify" [expr]
notNot _ = Nothing

-- De Morgan's laws:
-- (and (not a) (not b)) = (not (or a b))
-- (or (not a) (not b)) = (not (and a b))
deMorgan :: Expr -> Maybe Expr
deMorgan (FuncCall "and" [FuncCall "not" [lhs], FuncCall "not" [rhs]]) =
  Just $ FuncCall "not" [FuncCall "or" [lhs, rhs]]
deMorgan (FuncCall "or" [FuncCall "not" [lhs], FuncCall "not" [rhs]]) =
  Just $ FuncCall "not" [FuncCall "and" [lhs, rhs]]
deMorgan _ = Nothing

-- Constant folding for `not`
constNot :: Expr -> Maybe Expr
constNot (FuncCall "not" [Lit l]) = Just $ Lit $ VBool $ not $ toBool l
constNot _ = Nothing

-- Constant folding for `and`
constAnd :: Expr -> Maybe Expr
constAnd (FuncCall "and" args)
  | null unknown = Just $ Lit $ VBool $ and known
  | not $ and known = Just $ Lit $ VBool False
  | not $ null known = Just $ FuncCall "and" unknown
  | otherwise = Nothing
  where
    (unknown, known') = partitionMaybe getLit args
    known = toBool <$> known'
constAnd _ = Nothing

-- Constant folding for `or`
constOr :: Expr -> Maybe Expr
constOr (FuncCall "or" args)
  | null unknown = Just $ Lit $ VBool $ or known
  | or known = Just $ Lit $ VBool True
  | not $ null known = Just $ FuncCall "or" unknown
  | otherwise = Nothing
  where
    (unknown, known') = partitionMaybe getLit args
    known = toBool <$> known'
constOr _ = Nothing
