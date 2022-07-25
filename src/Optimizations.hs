{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Optimizations
  ( exprOptimizations
  , stmtOptimizations
  ) where

import Data.Semigroup (Any(..))
import Mid.Expr (Expr(..), Value(..), getLit, toBool, toNum)
import Mid.Proc (Statement(..))
import Utils.Maybe (partitionMaybe)

exprOptimizations :: [Expr -> Maybe Expr]
exprOptimizations =
  [notNot, deMorgan, constNot, constAnd, constOr, constPlus, constMinus]

stmtOptimizations :: [Statement -> Maybe Statement]
stmtOptimizations =
  [flattenDo, ifConstCondition, whileConstCondition, untilConstCondition]

-- Remove double `not`s
notNot :: Expr -> Maybe Expr
notNot (FuncCall "not" [FuncCall "not" [expr]]) =
  Just $ FuncCall "to-bool" [expr]
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
    (unknown, fmap toBool -> known) = partitionMaybe getLit args
constAnd _ = Nothing

-- Constant folding for `or`
constOr :: Expr -> Maybe Expr
constOr (FuncCall "or" args)
  | null unknown = Just $ Lit $ VBool $ or known
  | or known = Just $ Lit $ VBool True
  | not $ null known = Just $ FuncCall "or" unknown
  | otherwise = Nothing
  where
    (unknown, fmap toBool -> known) = partitionMaybe getLit args
constOr _ = Nothing

-- Constant folding for `+`
constPlus :: Expr -> Maybe Expr
constPlus (FuncCall "+" args)
  | null unknown = Just knownSum
  | length known > 1 = Just $ FuncCall "+" $ knownSum : unknown
  | otherwise = Nothing
  where
    (unknown, fmap toNum -> known) = partitionMaybe getLit args
    knownSum = Lit $ VNum $ sum known
constPlus _ = Nothing

-- Constant folding for `-`
constMinus :: Expr -> Maybe Expr
constMinus (FuncCall "-" [x]) = Lit . VNum . negate . toNum <$> getLit x
constMinus (FuncCall "-" (x:xs))
  | Just x' <- getLit x
  , Just xs' <- traverse getLit xs =
    Just $ Lit $ VNum $ toNum x' - sum (toNum <$> xs')
constMinus _ = Nothing

-- Flatten `Do` blocks
flattenDo :: Statement -> Maybe Statement
flattenDo (Do body)
  | didSomething = Just $ Do flattened
  | otherwise = Nothing
  where
    flatten (Do xs) = (Any True, xs)
    flatten x = (Any False, [x])
    (Any didSomething, flattened) = concat <$> traverse flatten body
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
