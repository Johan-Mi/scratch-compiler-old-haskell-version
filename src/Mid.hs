module Mid
  ( Program
  , mkProgram
  , stage
  , sprites
  , targets
  ) where

import Data.Function (on)
import Data.List (partition, sortOn)
import Data.List.NonEmpty (NonEmpty(..), groupBy)
import Data.Semigroup (sconcat)
import Lens.Micro (Lens', Traversal', (^.))
import LispAST (LispAST)
import Mid.Error (MidError(..))
import Mid.Sprite (Sprite, isStage, mkSprite, spriteName)

data Program =
  Program
    { _stage :: Sprite
    , _sprites :: [Sprite]
    }
  deriving (Show)

stage :: Lens' Program Sprite
stage f prg = (\sc -> prg {_stage = sc}) <$> f (_stage prg)

sprites :: Lens' Program [Sprite]
sprites f prg = (\spr -> prg {_sprites = spr}) <$> f (_sprites prg)

targets :: Traversal' Program Sprite
targets f prg =
  (\(sc :| spr) -> prg {_stage = sc, _sprites = spr}) <$>
  traverse f (_stage prg :| _sprites prg)

mkProgram :: [LispAST] -> Either MidError Program
mkProgram asts = do
  spritesAndStages <- traverse mkSprite asts
  let mergedSpritesAndStages =
        fmap sconcat $
        groupBy ((==) `on` (^. spriteName)) $
        sortOn (^. spriteName) spritesAndStages
  let (stages, sprites') = partition isStage mergedSpritesAndStages
  stage' <-
    case stages of
      [theStage] -> Right theStage
      _ -> Left NoStage
  return $ Program stage' sprites'
