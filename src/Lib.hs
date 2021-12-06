module Lib
  ( compileProgram
  ) where

import Control.Arrow (left)
import Control.Monad ((<=<))
import Control.Monad.Except (liftEither, liftIO, runExceptT, withExceptT)
import Error (IsError(..))
import Macro (expandMacros)
import Mid (mkProgram)
import Optimize (optimizeProgram)
import Parser (programP)
import SB3 (writeSB3File)
import Text.Parsec.Text (parseFromFile)

compileProgram :: FilePath -> IO ()
compileProgram path =
  either id pure <=< runExceptT $ do
    parsed <-
      liftEither . left printError =<< liftIO (parseFromFile programP path)
    expanded <- withExceptT printError $ expandMacros parsed
    prg <- liftEither $ left printError $ mkProgram expanded
    let optimized = optimizeProgram prg
    withExceptT printError $ writeSB3File optimized
