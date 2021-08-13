module Mid.Expr
  ( Expr(..)
  , Value(..)
  , mkExpr
  ) where

import qualified Data.Text as T
import LispAST (LispAST(..))
import Mid.Error (MidError(..))

data Value
  = VNum Double
  | VStr T.Text

data Expr
  = Lit Value
  | Sym T.Text
  | FuncCall T.Text [Expr]

mkExpr :: LispAST -> Either MidError Expr
mkExpr (LispNum n) = Right $ Lit $ VNum n
mkExpr (LispString s) = Right $ Lit $ VStr s
mkExpr (LispSym s) = Right $ Sym s
mkExpr (LispNode (LispSym name) args) = FuncCall name <$> traverse mkExpr args
mkExpr ast = Left $ NotAnExpression ast
