module Lib
  ( someFunc
  ) where

import Macro (expandMacros)
import Mid (mkProgram)
import Optimize (optimize)
import Parser (programP)
import Text.Parsec.Text (parseFromFile)

someFunc :: IO ()
someFunc = do
  Right a <- parseFromFile programP "program.scratch"
  let Right b = expandMacros a
  let Right c = mkProgram b
  let d = optimize c
  print d
