module Optimize
  ( optimize
  ) where

import Data.Foldable (asum)
import Lens.Micro ((%~), each, rewriteOf, transformOf)
import Mid (Program, targets)
import Mid.Expr (Expr, subExprs)
import Mid.Proc (Procedure(..), Statement, stmtExprs, subStmts)
import Mid.Sprite (Sprite, procedures)
import Optimizations (exprOptimizations, stmtOptimizations)
import Utils.Functor (flap)

class Optimizable a where
  optimize :: a -> a

instance Optimizable Program where
  optimize = targets . each %~ optimize

instance Optimizable Sprite where
  optimize = procedures . each %~ optimize

instance Optimizable Procedure where
  optimize (Procedure name params body) =
    Procedure name params (optimize <$> body)

instance Optimizable Statement where
  optimize =
    rewriteOf subStmts (asum . flap stmtOptimizations) .
    transformOf subStmts (stmtExprs %~ optimize)

instance Optimizable Expr where
  optimize = rewriteOf subExprs $ asum . flap exprOptimizations
