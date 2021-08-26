{-# LANGUAGE OverloadedStrings #-}

module Optimizations
  ( exprOptimizations
  , stmtOptimizations
  ) where

import Mid.Expr (Expr(..), Value(..), getLit, toBool)
import Mid.Proc (Statement(..))
import Utils.Maybe (partitionMaybe)

exprOptimizations :: [Expr -> Maybe Expr]
exprOptimizations = [notNot, deMorgan, constNot, constAnd, constOr]

stmtOptimizations :: [Statement -> Maybe Statement]
stmtOptimizations =
  [flattenDo, ifConstCondition, whileConstCondition, untilConstCondition]

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

-- Flatten `Do` blocks
flattenDo :: Statement -> Maybe Statement
flattenDo (Do [stmt]) = Just stmt
flattenDo _ = Nothing

-- Remove if statements with a constant condition
ifConstCondition :: Statement -> Maybe Statement
ifConstCondition (IfElse (Lit cond) true false) =
  Just $
  if toBool cond
    then true
    else false
ifConstCondition _ = Nothing

-- While loops with a constant condition do nothing or loop forever
whileConstCondition :: Statement -> Maybe Statement
whileConstCondition (While (Lit cond) body) =
  Just $
  if toBool cond
    then Forever body
    else Do []
whileConstCondition _ = Nothing

-- Until loops with a constant condition do nothing or loop forever
untilConstCondition :: Statement -> Maybe Statement
untilConstCondition (Until (Lit cond) body) =
  Just $
  if toBool cond
    then Do []
    else Forever body
untilConstCondition _ = Nothing
