{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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
  deriving (Show)

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
  (name, ) <$> traverse mkExpr params
mkProcSignature ast = Left $ InvalidProcSignature ast

data Statement
  = ProcCall T.Text [Expr]
  | Do [Statement]
  | IfElse Expr Statement Statement
  | Repeat Expr [Statement]
  | Forever [Statement]
  | Until Expr [Statement]
  | While Expr [Statement]
  | For Expr Expr [Statement]
  deriving (Show)

mkStatement :: LispAST -> Either MidError Statement
mkStatement (LispNode (LispSym name) args) =
  case name `lookup` specialStatements of
    Just fun -> fun args
    Nothing -> ProcCall name <$> traverse mkExpr args
mkStatement ast = Left $ NotAStatement ast

specialStatements :: [(T.Text, [LispAST] -> Either MidError Statement)]
specialStatements =
  [ ("if", stmtIfElse)
  , ("do", stmtDo)
  , ("repeat", stmtRepeat)
  , ("forever", stmtForever)
  , ("until", stmtUntil)
  , ("while", stmtWhile)
  , ("for", stmtFor)
  ]

stmtIfElse :: [LispAST] -> Either MidError Statement
stmtIfElse [cond, true, false] =
  IfElse <$> mkExpr cond <*> mkStatement true <*> mkStatement false

stmtDo :: [LispAST] -> Either MidError Statement
stmtDo = fmap Do . traverse mkStatement

stmtRepeat :: [LispAST] -> Either MidError Statement
stmtRepeat (times:body) = Repeat <$> mkExpr times <*> traverse mkStatement body

stmtForever :: [LispAST] -> Either MidError Statement
stmtForever = fmap Forever . traverse mkStatement

stmtUntil :: [LispAST] -> Either MidError Statement
stmtUntil (cond:body) = Until <$> mkExpr cond <*> traverse mkStatement body

stmtWhile :: [LispAST] -> Either MidError Statement
stmtWhile (cond:body) = While <$> mkExpr cond <*> traverse mkStatement body

stmtFor :: [LispAST] -> Either MidError Statement
stmtFor (var:times:body) =
  For <$> mkExpr var <*> mkExpr times <*> traverse mkStatement body
