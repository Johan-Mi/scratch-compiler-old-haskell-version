{-# LANGUAGE OverloadedStrings #-}

module Mid.Sprite
  ( Sprite
  , mkSprite
  , isStage
  , procedures
  , spriteName
  , costumes
  , variables
  , lists
  ) where

import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Lens.Micro (Lens')
import LispAST (LispAST(..), asTheFunction, getSym)
import Mid.Error (MidError(..))
import Mid.Proc (Procedure, mkProc)
import Utils.Either (justFailWith, maybeToRight)
import Utils.Maybe (partitionMaybe)

data Sprite =
  Sprite
    { _name :: T.Text
    , _costumes :: [(T.Text, FilePath)]
    , _variables :: [T.Text]
    , _lists :: [T.Text]
    , _procedures :: [Procedure]
    }
  deriving (Show)

spriteName :: Lens' Sprite T.Text
spriteName f spr = (\name -> spr {_name = name}) <$> f (_name spr)

costumes :: Lens' Sprite [(T.Text, FilePath)]
costumes f spr = (\csts -> spr {_costumes = csts}) <$> f (_costumes spr)

procedures :: Lens' Sprite [Procedure]
procedures f spr = (\procs -> spr {_procedures = procs}) <$> f (_procedures spr)

variables :: Lens' Sprite [T.Text]
variables f spr = (\vars -> spr {_variables = vars}) <$> f (_variables spr)

lists :: Lens' Sprite [T.Text]
lists f spr = (\lsts -> spr {_lists = lsts}) <$> f (_lists spr)

isStage :: Sprite -> Bool
isStage = (== "Stage") . _name

mkSprite :: LispAST -> Either MidError Sprite
mkSprite (LispNode (LispSym "sprite") (LispString name':args)) = do
  let (args1, costumeLists) = partitionMaybe mkCostumeList args
  let (args2, varDecls) = partitionMaybe mkVarDecl args1
  let (args3, listDecls) = partitionMaybe mkListDecl args2
  let (remaining, procDefs) = partitionMaybe mkProc args3
  justFailWith InvalidItemInSprite $ listToMaybe remaining
  costumes' <- concat <$> sequenceA costumeLists
  vars <- concat <$> sequenceA varDecls
  lists' <- concat <$> sequenceA listDecls
  procedures' <- sequenceA procDefs
  return $ Sprite name' costumes' vars lists' procedures'
mkSprite (LispNode (LispSym "sprite") _) = Left SpriteLacksName
mkSprite nonSprite = Left $ NonSpriteAtTopLevel nonSprite

mkCostumeList :: LispAST -> Maybe (Either MidError [(T.Text, FilePath)])
mkCostumeList = f `asTheFunction` "costumes"
  where
    f [] = Right []
    f [LispString name] = Left $ CostumeLacksFilePath name
    f (LispString name:LispString path:xs) = ((name, T.unpack path) :) <$> f xs
    f _ = Left NonStringInCostumeList

mkVarDecl :: LispAST -> Maybe (Either MidError [T.Text])
mkVarDecl =
  (maybeToRight NonSymbolInVarDecl . traverse getSym) `asTheFunction`
  "variables"

mkListDecl :: LispAST -> Maybe (Either MidError [T.Text])
mkListDecl =
  (maybeToRight NonSymbolInListDecl . traverse getSym) `asTheFunction` "lists"
