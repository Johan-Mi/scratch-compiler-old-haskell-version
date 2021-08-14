module Lib
  ( someFunc
  ) where

import Macro (expandMacros)
import Mid (mkProgram)
import Parser (programP)
import Text.Parsec.Text (parseFromFile)

someFunc :: IO ()
someFunc = do
  Right a <- parseFromFile programP "program.scratch"
  let Right b = expandMacros a
  let c = mkProgram b
  print c
