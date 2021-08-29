module Mid
  ( Program
  , mkProgram
  , stage
  , sprites
  ) where

import Data.List (partition)
import Lens.Micro (Lens')
import LispAST (LispAST)
import Mid.Error (MidError(..))
import Mid.Sprite (Sprite, isStage, mkSprite)

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

mkProgram :: [LispAST] -> Either MidError Program
mkProgram asts = do
  spritesAndStages <- traverse mkSprite asts
  let (stages, sprites') = partition isStage spritesAndStages
  stage' <-
    case stages of
      [theStage] -> Right theStage
      xs -> Left $ WrongStageCount $ length xs
  return $ Program stage' sprites'
