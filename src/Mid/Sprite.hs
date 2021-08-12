{-# LANGUAGE OverloadedStrings #-}

module Mid.Sprite
  ( Sprite
  , mkSprite
  , isStage
  ) where

import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import LispAST (LispAST(..), asTheFunction, getSym)
import Mid.Error (MidError(..))
import Mid.Proc (Procedure, mkProc)
import Utils.Either (justFailWith, maybeToRight)
import Utils.Maybe (partitionMaybe)

data Sprite =
  Sprite
    { name :: T.Text
    , costumes :: [T.Text]
    , variables :: [T.Text]
    , lists :: [T.Text]
    , procedures :: [Procedure]
    }

isStage :: Sprite -> Bool
isStage = (== "Stage") . name

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
