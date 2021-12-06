{-# LANGUAGE OverloadedStrings #-}

module Block.Error
  ( BlockError(..)
  , ArgCount(..)
  ) where

import qualified Data.Text as T
import Error (IsError(..))

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

instance IsError BlockError where
  showError (InvalidParamsForSpecialProcDef procName) =
    "invalid arguments for definition of special procedure `" <> procName <> "`"
  showError (UnknownProc procName) = "unknown procedure `" <> procName <> "`"
  showError (InvalidArgsForBuiltinProc procName) =
    "invalid arguments for call to builtin procedure `" <> procName <> "`"
  showError (InvalidArgsForBuiltinFunc funcName) =
    "invalid arguments for call to builtin function `" <> funcName <> "`"
  showError (FuncWrongArgCount name expected got) =
    "function `" <>
    name <>
    "` expected " <>
    T.pack (show expected) <> " arguments but got " <> T.pack (show got)
  showError (UnknownSymbolInExpr name) =
    "unknown symbol `" <> name <> "` used in an expression"
  showError (NonSymbolInProcDef name) =
    "non-symbol in definition of procedure `" <> name <> "`"
  showError (UnknownFunc name) = "unknown function `" <> name <> "`"
  showError (VarDoesntExist name) = "variable `" <> name <> "` does not exist"
  showError (ListDoesntExist name) = "list `" <> name <> "` does not exist"

data ArgCount
  = Exactly Int
  | AtLeast Int

instance Show ArgCount where
  show (Exactly num) = show num
  show (AtLeast num) = "at least " ++ show num
