module Mid.Expr
  ( Expr(..)
  , Value(..)
  , mkExpr
  ) where

import qualified Data.Text as T
import LispAST (LispAST(..))

data Value
  = VNum Double
  | VStr T.Text

data Expr
  = Lit Value
  | Sym T.Text
  | FuncCall T.Text [Expr]

mkExpr :: LispAST -> Expr
mkExpr (LispNum n) = Lit $ VNum n
mkExpr (LispString s) = Lit $ VStr s
mkExpr (LispSym s) = Sym s
