module Mid
  ( Program
  , mkProgram
  , scene
  , sprites
  ) where

import Data.List (partition)
import Lens.Micro (Lens')
import LispAST (LispAST)
import Mid.Error (MidError(..))
import Mid.Sprite (Sprite, isStage, mkSprite)

data Program =
  Program
    { _scene :: Sprite
    , _sprites :: [Sprite]
    }
  deriving (Show)

scene :: Lens' Program Sprite
scene f prg = (\sc -> prg {_scene = sc}) <$> f (_scene prg)

sprites :: Lens' Program [Sprite]
sprites f prg = (\spr -> prg {_sprites = spr}) <$> f (_sprites prg)

mkProgram :: [LispAST] -> Either MidError Program
mkProgram asts = do
  spritesAndStages <- traverse mkSprite asts
  let (stages, sprites') = partition isStage spritesAndStages
  stage <-
    case stages of
      [theStage] -> Right theStage
      xs -> Left $ WrongStageCount $ length xs
  return $ Program stage sprites'
