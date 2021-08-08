{-# LANGUAGE OverloadedStrings #-}

module Mid
  (
  ) where

import Data.List (partition)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import LispAST (LispAST(..), getSym)
import Text.Printf (printf)
import Utils.Either (justFailWith, maybeToRight)
import Utils.Maybe (partitionMaybe)
import Utils.Text (cropTextTo)

data MidError
  = NonSpriteAtTopLevel LispAST
  | WrongStageCount Int
  | NonSymbolInVarDecl
  | NonSymbolInListDecl
  | InvalidItemInSprite LispAST
  | SpriteLacksName
  | NonStringInCostumeList

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

data Program =
  Program
    { scene :: Sprite
    , sprites :: [Sprite]
    }

data Sprite =
  Sprite
    { name :: T.Text
    , costumes :: [T.Text]
    , variables :: [T.Text]
    , lists :: [T.Text]
    , procedures :: [Procedure]
    }

data Procedure =
  Procedure T.Text [Expr] [Statement]

data Statement
  = ProcCall T.Text [Expr]
  | IfElse Expr Statement Statement

data Value
  = VNum Double
  | VStr T.Text

data Expr
  = Lit Value
  | Sym T.Text
  | FuncCall T.Text [Expr]

mkSprite :: LispAST -> Either MidError Sprite
mkSprite (LispNode (LispSym "sprite") (name:args)) = do
  name <- maybeToRight SpriteLacksName $ getSym name
  let (args, costumeLists) = partitionMaybe mkCostumeList args
  let (args, varDecls) = partitionMaybe mkVarDecl args
  let (args, listDecls) = partitionMaybe mkListDecl args
  let (remaining, procDefs) = partitionMaybe mkProc args
  justFailWith InvalidItemInSprite $ listToMaybe remaining
  costumes <- concat <$> sequenceA costumeLists
  vars <- concat <$> sequenceA varDecls
  lists <- concat <$> sequenceA listDecls
  procs <- sequenceA procDefs
  return $ Sprite name costumes vars lists procs
mkSprite (LispNode (LispSym "sprite") []) = Left SpriteLacksName
mkSprite nonSprite = Left $ NonSpriteAtTopLevel nonSprite

mkCostumeList :: LispAST -> Maybe (Either MidError [T.Text])
mkCostumeList (LispNode (LispSym "costumes") costumes) =
  Just $ maybeToRight NonStringInCostumeList $ traverse f costumes
  where
    f (LispString str) = Just str
    f _ = Nothing
mkCostumeList _ = Nothing

mkVarDecl :: LispAST -> Maybe (Either MidError [T.Text])
mkVarDecl (LispNode (LispSym "variables") vars) =
  Just $ maybeToRight NonSymbolInVarDecl $ traverse getSym vars
mkVarDecl _ = Nothing

mkListDecl :: LispAST -> Maybe (Either MidError [T.Text])
mkListDecl (LispNode (LispSym "lists") lists) =
  Just $ maybeToRight NonSymbolInListDecl $ traverse getSym lists
mkListDecl _ = Nothing

mkProc :: LispAST -> Maybe (Either MidError Procedure)
mkProc = undefined

isStage :: Sprite -> Bool
isStage = (== "Stage") . name

mkProgram :: [LispAST] -> Either MidError Program
mkProgram asts = do
  spritesAndStages <- traverse mkSprite asts
  let (stages, sprites) = partition isStage spritesAndStages
  stage <-
    case stages of
      [theStage] -> Right theStage
      xs -> Left $ WrongStageCount $ length xs
  return $ Program stage sprites
