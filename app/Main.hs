module Main where

import Lib (compileProgram)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  path <-
    case args of
      [path'] -> pure path'
      [] -> pure "program.scratch"
      _ -> putStrLn "Too many command line arguments" >> exitFailure
  compileProgram path
