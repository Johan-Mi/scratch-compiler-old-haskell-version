module Mid.Error
  ( MidError(..)
  ) where

import qualified Data.Text as T
import LispAST (LispAST)
import Text.Printf (printf)
import Utils.Text (cropTextTo)

data MidError
  = NonSpriteAtTopLevel LispAST
  | WrongStageCount Int
  | NonSymbolInVarDecl
  | NonSymbolInListDecl
  | InvalidItemInSprite LispAST
  | SpriteLacksName
  | NonStringInCostumeList
  | InvalidProcSignature LispAST
  | ProcDefLacksSignature
  | NotAStatement LispAST
  | NotAnExpression LispAST

instance Show MidError where
  show (NonSpriteAtTopLevel ast) =
    printf "non-sprite at top level of program: `%s`" $
    cropTextTo 30 $ T.pack $ show ast
  show (WrongStageCount count) =
    printf "%d stages found instead of exactly one" count
  show NonSymbolInVarDecl = "non-symbol in variable declaration"
  show NonSymbolInListDecl = "non-symbol in list declaration"
  show (InvalidItemInSprite ast) =
    printf "invalid item in sprite: `%s`" $ cropTextTo 30 $ T.pack $ show ast
  show SpriteLacksName = "sprite lacks name"
  show NonStringInCostumeList = "non-string in costume list"
  show (InvalidProcSignature ast) =
    printf "invalid procedure signature: `%s`" $
    cropTextTo 30 $ T.pack $ show ast
  show ProcDefLacksSignature = "procedure definition lacks a signature"
  show (NotAStatement ast) =
    printf "not a statement: `%s`" $ cropTextTo 30 $ T.pack $ show ast
  show (NotAnExpression ast) =
    printf "not an expression: `%s`" $ cropTextTo 30 $ T.pack $ show ast
