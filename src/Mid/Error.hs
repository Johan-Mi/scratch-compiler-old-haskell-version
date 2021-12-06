{-# LANGUAGE OverloadedStrings #-}

module Mid.Error
  ( MidError(..)
  ) where

import qualified Data.Text as T
import Error (IsError(..))
import LispAST (LispAST)
import Utils.Text (cropTextTo)

data MidError
  = NonSpriteAtTopLevel LispAST
  | NoStage
  | NonSymbolInVarDecl
  | NonSymbolInListDecl
  | InvalidItemInSprite LispAST
  | SpriteLacksName
  | NonStringInCostumeList
  | CostumeLacksFilePath T.Text
  | InvalidProcSignature LispAST
  | ProcDefLacksSignature
  | NotAStatement LispAST
  | NotAnExpression LispAST
  | InvalidArgumentsFor T.Text

instance IsError MidError where
  showError (NonSpriteAtTopLevel ast) =
    "non-sprite at top level of program: `" <> item <> "`"
    where
      item = cropTextTo 30 $ T.pack $ show ast
  showError NoStage = "program has no stage"
  showError NonSymbolInVarDecl = "non-symbol in variable declaration"
  showError NonSymbolInListDecl = "non-symbol in list declaration"
  showError (InvalidItemInSprite ast) =
    "invalid item in sprite: `" <> item <> "`"
    where
      item = cropTextTo 30 $ T.pack $ show ast
  showError SpriteLacksName = "sprite lacks name"
  showError NonStringInCostumeList = "non-string in costume list"
  showError (CostumeLacksFilePath name) =
    "costume `" <> name <> "` lacks a file path"
  showError (InvalidProcSignature ast) =
    "invalid procedure signature: `" <> item <> "`"
    where
      item = cropTextTo 30 $ T.pack $ show ast
  showError ProcDefLacksSignature = "procedure definition lacks a signature"
  showError (NotAStatement ast) = "not a statement: `" <> item <> "`"
    where
      item = cropTextTo 30 $ T.pack $ show ast
  showError (NotAnExpression ast) = "not an expression: `" <> item <> "`"
    where
      item = cropTextTo 30 $ T.pack $ show ast
  showError (InvalidArgumentsFor name) =
    "invalid arguments for `" <> name <> "`"
