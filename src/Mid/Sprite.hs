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

import qualified Data.Text as T
import Lens.Micro (Lens')
import LispAST (LispAST(..), asTheFunction)
import Mid.Error (MidError(..))
import Mid.Proc (Procedure, mkListDecl, mkProc, mkVarDecl)
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

instance Semigroup Sprite where
  Sprite nm co va li pr <> Sprite _nm' co' va' li' pr' =
    Sprite nm (co <> co') (va <> va') (li <> li') (pr <> pr')

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
      (args2, varDecls) = partitionMaybe mkVarDecl args1
      (args3, listDecls) = partitionMaybe mkListDecl args2
      (remaining, procDefs) = partitionMaybe mkProc args3
  case remaining of
    [] -> pure ()
    (item:_) -> Left $ InvalidItemInSprite item
  costumes' <- concat <$> sequenceA costumeLists
  vars <- concat <$> sequenceA varDecls
  lists' <- concat <$> sequenceA listDecls
  procedures' <- sequenceA procDefs
  pure $ Sprite name' costumes' vars lists' procedures'
mkSprite (LispNode (LispSym "sprite") _) = Left SpriteLacksName
mkSprite nonSprite = Left $ NonSpriteAtTopLevel nonSprite

mkCostumeList :: LispAST -> Maybe (Either MidError [(T.Text, FilePath)])
mkCostumeList = f `asTheFunction` "costumes"
  where
    f [] = Right []
    f [LispString name] = Left $ CostumeLacksFilePath name
    f (LispString name:LispString path:xs) = ((name, T.unpack path) :) <$> f xs
    f _ = Left NonStringInCostumeList
