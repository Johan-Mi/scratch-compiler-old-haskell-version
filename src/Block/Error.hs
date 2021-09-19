module Block.Error
  ( BlockError(..)
  , ArgCount(..)
  ) where

import qualified Data.Text as T
import Text.Printf (printf)

data BlockError
  = InvalidParamsForSpecialProcDef T.Text
  | UnknownProc T.Text
  | InvalidArgsForBuiltinProc T.Text
  | InvalidArgsForBuiltinFunc T.Text
  | FuncWrongArgCount T.Text ArgCount Int
  | UnknownSymbolInExpr T.Text
  | NonSymbolInProcDef T.Text
  | UnknownFunc T.Text
  | VarDoesntExist T.Text
  | ListDoesntExist T.Text

instance Show BlockError where
  show (InvalidParamsForSpecialProcDef procName) =
    printf "invalid arguments for definition of special procedure `%s`" procName
  show (UnknownProc procName) = printf "unknown procedure `%s`" procName
  show (InvalidArgsForBuiltinProc procName) =
    printf "invalid arguments for call to builtin procedure `%s`" procName
  show (InvalidArgsForBuiltinFunc funcName) =
    printf "invalid arguments for call to builtin function `%s`" funcName
  show (FuncWrongArgCount name expected got) =
    printf
      "function `%s` expected %s arguments but got %d"
      name
      (show expected)
      got
  show (UnknownSymbolInExpr name) =
    printf "unknown symbol `%s` used in an expression" name
  show (NonSymbolInProcDef name) =
    printf "non-symbol in definition of procedure `%s`" name
  show (UnknownFunc name) = printf "unknown function `%s`" name
  show (VarDoesntExist name) = printf "variable `%s` does not exist" name
  show (ListDoesntExist name) = printf "list `%s` does not exist" name

data ArgCount
  = Exactly Int
  | AtLeast Int

instance Show ArgCount where
  show (Exactly num) = show num
  show (AtLeast num) = "at least " ++ show num
