module Mid
  ( Program
  , mkProgram
  ) where

import Data.List (partition)
import LispAST (LispAST)
import Mid.Error (MidError(..))
import Mid.Sprite (Sprite, isStage, mkSprite)

data Program =
  Program
    { scene :: Sprite
    , sprites :: [Sprite]
    }
  deriving (Show)

mkProgram :: [LispAST] -> Either MidError Program
mkProgram asts = do
  spritesAndStages <- traverse mkSprite asts
  let (stages, sprites') = partition isStage spritesAndStages
  stage <-
    case stages of
      [theStage] -> Right theStage
      xs -> Left $ WrongStageCount $ length xs
  return $ Program stage sprites'
