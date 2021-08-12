{-# LANGUAGE OverloadedStrings #-}

module Mid
  (
  ) where

import Data.List (partition)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import LispAST (LispAST(..), asTheFunction, getSym)
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
  | InvalidProcSignature LispAST
  | ProcDefLacksSignature

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
mkSprite (LispNode (LispSym "sprite") (LispString name:args)) = do
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
mkSprite (LispNode (LispSym "sprite") _) = Left SpriteLacksName
mkSprite nonSprite = Left $ NonSpriteAtTopLevel nonSprite

mkCostumeList :: LispAST -> Maybe (Either MidError [T.Text])
mkCostumeList =
  (maybeToRight NonStringInCostumeList . traverse f) `asTheFunction` "costumes"
  where
    f (LispString str) = Just str
    f _ = Nothing

mkVarDecl :: LispAST -> Maybe (Either MidError [T.Text])
mkVarDecl =
  (maybeToRight NonSymbolInVarDecl . traverse getSym) `asTheFunction`
  "variables"

mkListDecl :: LispAST -> Maybe (Either MidError [T.Text])
mkListDecl =
  (maybeToRight NonSymbolInListDecl . traverse getSym) `asTheFunction` "lists"

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

mkStatement :: LispAST -> Either MidError Statement
mkStatement = undefined

mkExpr :: LispAST -> Expr
mkExpr (LispNum n) = Lit $ VNum n
mkExpr (LispString s) = Lit $ VStr s
mkExpr (LispSym s) = Sym s

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
