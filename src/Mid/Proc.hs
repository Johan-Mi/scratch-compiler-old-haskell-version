{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Mid.Proc
  ( Procedure(..)
  , procedureName
  , mkProc
  , Statement(..)
  , subStmts
  , stmtExprs
  ) where

import Data.Functor ((<&>))
import qualified Data.Text as T
import Lens.Micro (Lens', Traversal')
import LispAST (LispAST(..), asTheFunction)
import Mid.Error (MidError(..))
import Mid.Expr (Expr, mkExpr)

data Procedure =
  Procedure T.Text [Expr] [Statement]
  deriving (Show)

procedureName :: Lens' Procedure T.Text
procedureName f (Procedure name params body) =
  (\name' -> Procedure name' params body) <$> f name

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
stmtIfElse [cond, true] =
  IfElse <$> mkExpr cond <*> mkStatement true <&> ($ Do [])
stmtIfElse _ = Left $ InvalidArgumentsFor "if"

stmtDo :: [LispAST] -> Either MidError Statement
stmtDo = fmap Do . traverse mkStatement

stmtRepeat :: [LispAST] -> Either MidError Statement
stmtRepeat (times:body) = Repeat <$> mkExpr times <*> traverse mkStatement body
stmtRepeat _ = Left $ InvalidArgumentsFor "repeat"

stmtForever :: [LispAST] -> Either MidError Statement
stmtForever = fmap Forever . traverse mkStatement

stmtUntil :: [LispAST] -> Either MidError Statement
stmtUntil (cond:body) = Until <$> mkExpr cond <*> traverse mkStatement body
stmtUntil _ = Left $ InvalidArgumentsFor "until"

stmtWhile :: [LispAST] -> Either MidError Statement
stmtWhile (cond:body) = While <$> mkExpr cond <*> traverse mkStatement body
stmtWhile _ = Left $ InvalidArgumentsFor "while"

stmtFor :: [LispAST] -> Either MidError Statement
stmtFor (var:times:body) =
  For <$> mkExpr var <*> mkExpr times <*> traverse mkStatement body
stmtFor _ = Left $ InvalidArgumentsFor "for"

subStmts :: Traversal' Statement Statement
subStmts _ pc@(ProcCall _ _) = pure pc
subStmts f (Do stmts) = Do <$> traverse f stmts
subStmts f (IfElse cond true false) = IfElse cond <$> f true <*> f false
subStmts f (Repeat times body) = Repeat times <$> traverse f body
subStmts f (Forever body) = Forever <$> traverse f body
subStmts f (Until cond body) = Until cond <$> traverse f body
subStmts f (While cond body) = While cond <$> traverse f body
subStmts f (For var times body) = For var times <$> traverse f body

stmtExprs :: Traversal' Statement Expr
stmtExprs f stmt =
  case stmt of
    (ProcCall name args) -> ProcCall name <$> traverse f args
    (Do stmts) -> pure $ Do stmts
    (IfElse cond true false) -> IfElse <$> f cond <&> ($ true) <&> ($ false)
    (Repeat times body) -> Repeat <$> f times <&> ($ body)
    (Forever body) -> pure $ Forever body
    (Until cond body) -> Until <$> f cond <&> ($ body)
    (While cond body) -> While <$> f cond <&> ($ body)
    (For var times body) -> For <$> f var <*> f times <&> ($ body)
