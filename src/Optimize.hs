module Optimize
  ( optimizeProgram
  ) where

import Data.Monoid (First(..))
import Lens.Micro ((%~), each, rewriteOf, transformOf)
import Mid (Program, targets)
import Mid.Expr (Expr, subExprs)
import Mid.Proc (Statement, procedureBody, stmtExprs, subStmts)
import Mid.Sprite (procedures)
import Optimizations (exprOptimizations, stmtOptimizations)

optimizeProgram :: Program -> Program
optimizeProgram =
  targets . procedures . each . procedureBody %~ optimizeStatement

optimizeStatement :: Statement -> Statement
optimizeStatement =
  rewriteOf subStmts (getFirst . foldMap (First .) stmtOptimizations) .
  transformOf subStmts (stmtExprs %~ optimizeExpr)

optimizeExpr :: Expr -> Expr
optimizeExpr =
  rewriteOf subExprs $ getFirst . foldMap (First .) exprOptimizations
