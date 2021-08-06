{-# LANGUAGE OverloadedStrings #-}

module MacroSpec
  ( spec
  ) where

import Macro
import Parser (programP)
import Test.Hspec
import qualified Text.Parsec

spec :: Spec
spec = do
  let parse = Text.Parsec.parse programP ""
  describe "expandMacros" $ do
    let input ==> output =
          expandMacros <$> parse input `shouldBe` Right <$> parse output
    it "expands symbol macros" $ do
      "(macro hello (say \"Hello\")) hello" ==> "(say \"Hello\")"
      "(macro puts print) (puts (* 2 5))" ==> "(print (* 2 5))"
      "(macro five 5) (+ 1 five)" ==> "(+ 1 5)"
    it "expands function macros" $ do
      "(macro (!= lhs rhs) (not (= ,lhs ,rhs))) (print (!= 1 2))" ==>
        "(print (not (= 1 2)))"
      "(macro (zero? val) (= ,val zero)) (and true (zero? 94))" ==>
        "(and true (= 94 zero))"
