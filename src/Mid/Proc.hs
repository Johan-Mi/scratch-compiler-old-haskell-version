{-# LANGUAGE OverloadedStrings #-}

module Mid.Proc
  ( Procedure
  , mkProc
  ) where

import qualified Data.Text as T
import LispAST (LispAST(..), asTheFunction)
import Mid.Error (MidError(..))
import Mid.Expr (Expr, mkExpr)

data Procedure =
  Procedure T.Text [Expr] [Statement]

mkProc :: LispAST -> Maybe (Either MidError Procedure)
mkProc = f `asTheFunction` "proc"
  where
    f [] = Left ProcDefLacksSignature
    f (sig:stmts) = do
      (name, params) <- mkProcSignature sig
      body <- traverse mkStatement stmts
      return $ Procedure name params body

mkProcSignature :: LispAST -> Either MidError (T.Text, [Expr])
mkProcSignature (LispNode (LispSym name) params) =
  Right (name, mkExpr <$> params)
mkProcSignature ast = Left $ InvalidProcSignature ast

data Statement
  = ProcCall T.Text [Expr]
  | IfElse Expr Statement Statement

mkStatement :: LispAST -> Either MidError Statement
mkStatement = undefined
