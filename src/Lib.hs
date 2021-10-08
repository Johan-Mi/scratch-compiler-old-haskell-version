module Lib
  ( compileProgram
  ) where

import Control.Arrow (left)
import Control.Monad ((<=<))
import Control.Monad.Except (liftEither, liftIO, runExceptT, withExceptT)
import Macro (expandMacros)
import Mid (mkProgram)
import Optimize (optimize)
import Parser (programP)
import SB3 (writeSB3File)
import Text.Parsec.Text (parseFromFile)

compileProgram :: FilePath -> IO ()
compileProgram path =
  either id return <=< runExceptT $ do
    parsed <- liftEither . left print =<< liftIO (parseFromFile programP path)
    expanded <- liftEither $ left print $ expandMacros parsed
    prg <- liftEither $ left print $ mkProgram expanded
    let optimized = optimize prg
    withExceptT print $ writeSB3File optimized
