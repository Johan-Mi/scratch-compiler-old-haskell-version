{-# LANGUAGE OverloadedStrings #-}

module MacroSpec
  ( spec
  ) where

import Control.Arrow (left)
import Control.Monad.Except (runExceptT)
import Error (showError)
import Macro
import Parser (programP)
import Test.Hspec
import qualified Text.Parsec

spec :: Spec
spec = do
  let parse = left showError . Text.Parsec.parse programP ""
  describe "expandMacros" $ do
    let input ==> output =
          either
            (pure . Left)
            (fmap (left showError) . runExceptT . expandMacros)
            (parse input) `shouldReturn`
          parse output
    it "expands symbol macros" $ do
      "(macro hello (say \"Hello\")) hello" ==> "(say \"Hello\")"
      "(macro puts print) (puts (* 2 5))" ==> "(print (* 2 5))"
      "(macro five 5) (+ 1 five)" ==> "(+ 1 5)"
    it "expands function macros" $ do
      "(macro (!= lhs rhs) (not (= ,lhs ,rhs))) (print (!= 1 2))" ==>
        "(print (not (= 1 2)))"
      "(macro (zero? val) (= ,val zero)) (and true (zero? 94))" ==>
        "(and true (= 94 zero))"
