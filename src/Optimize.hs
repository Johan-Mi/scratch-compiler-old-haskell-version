module Optimize
  ( optimize
  ) where

import Data.Foldable (asum)
import Lens.Micro ((%~), each, rewriteOf)
import Mid (Program, scene, sprites)
import Mid.Expr (Expr, subExprs)
import Mid.Proc (Procedure(..), Statement)
import Mid.Sprite (Sprite, procedures)
import Optimizations (exprOptimizations)
import Utils.Functor (flap)

class Optimizable a where
  optimize :: a -> a

instance Optimizable Program where
  optimize = (scene %~ optimize) . (sprites . each %~ optimize)

instance Optimizable Sprite where
  optimize = procedures . each %~ optimize

instance Optimizable Procedure where
  optimize (Procedure name params body) =
    Procedure name params (optimize <$> body)

instance Optimizable Statement where
  optimize = id -- TODO

instance Optimizable Expr where
  optimize = rewriteOf subExprs $ asum . flap exprOptimizations
