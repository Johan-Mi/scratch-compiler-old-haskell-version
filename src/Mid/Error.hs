module Mid.Error
  ( MidError(..)
  ) where

import qualified Data.Text as T
import LispAST (LispAST)
import Text.Printf (printf)
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

instance Show MidError where
  show (NonSpriteAtTopLevel ast) =
    printf "non-sprite at top level of program: `%s`" $
    cropTextTo 30 $ T.pack $ show ast
  show NoStage = "program has no stage"
  show NonSymbolInVarDecl = "non-symbol in variable declaration"
  show NonSymbolInListDecl = "non-symbol in list declaration"
  show (InvalidItemInSprite ast) =
    printf "invalid item in sprite: `%s`" $ cropTextTo 30 $ T.pack $ show ast
  show SpriteLacksName = "sprite lacks name"
  show NonStringInCostumeList = "non-string in costume list"
  show (CostumeLacksFilePath name) =
    printf "costume `%s` lacks a file path" name
  show (InvalidProcSignature ast) =
    printf "invalid procedure signature: `%s`" $
    cropTextTo 30 $ T.pack $ show ast
  show ProcDefLacksSignature = "procedure definition lacks a signature"
  show (NotAStatement ast) =
    printf "not a statement: `%s`" $ cropTextTo 30 $ T.pack $ show ast
  show (NotAnExpression ast) =
    printf "not an expression: `%s`" $ cropTextTo 30 $ T.pack $ show ast
  show (InvalidArgumentsFor name) = printf "invalid arguments for `%s`" name
