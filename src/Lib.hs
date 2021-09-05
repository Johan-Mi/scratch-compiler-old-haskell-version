module Lib
  ( someFunc
  ) where

import Control.Monad.Except
  ( ExceptT
  , liftEither
  , liftIO
  , runExceptT
  , withExceptT
  )
import Macro (expandMacros)
import Mid (mkProgram)
import Optimize (optimize)
import Parser (programP)
import SB3 (writeSB3File)
import Text.Parsec.Text (parseFromFile)

compileProgram :: FilePath -> ExceptT String IO ()
compileProgram path = do
  parsed <-
    withExceptT show $ liftEither =<< liftIO (parseFromFile programP path)
  expanded <- withExceptT show $ liftEither $ expandMacros parsed
  prg <- withExceptT show $ liftEither $ mkProgram expanded
  let optimized = optimize prg
  withExceptT show $ writeSB3File optimized

someFunc :: IO ()
someFunc = do
  either id return =<<
    runExceptT (withExceptT putStrLn $ compileProgram "program.scratch")
