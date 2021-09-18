module Optimize
  ( optimize
  ) where

import Data.Monoid (First(..))
import Lens.Micro ((%~), each, rewriteOf, transformOf)
import Mid (Program, targets)
import Mid.Expr (Expr, subExprs)
import Mid.Proc (Procedure(..), Statement, stmtExprs, subStmts)
import Mid.Sprite (Sprite, procedures)
import Optimizations (exprOptimizations, stmtOptimizations)

class Optimizable a where
  optimize :: a -> a

instance Optimizable Program where
  optimize = targets %~ optimize

instance Optimizable Sprite where
  optimize = procedures . each %~ optimize

instance Optimizable Procedure where
  optimize (Procedure name params body) =
    Procedure name params (optimize <$> body)

instance Optimizable Statement where
  optimize =
    rewriteOf subStmts (getFirst . foldMap (First .) stmtOptimizations) .
    transformOf subStmts (stmtExprs %~ optimize)

instance Optimizable Expr where
  optimize = rewriteOf subExprs $ getFirst . foldMap (First .) exprOptimizations
