{-# LANGUAGE OverloadedStrings #-}

module ParserSpec
  ( spec
  ) where

import Data.Either (isLeft)
import LispAST (LispAST(..))
import Parser
import Test.Hspec
import Text.Parsec (eof, parse)

spec :: Spec
spec = do
  describe "numP" $ do
    let shouldFail input = parse (numP <* eof) "" input `shouldSatisfy` isLeft
    let input ==> output =
          parse (numP <* eof) "" input `shouldBe` Right (LispNum output)
    it "fails on invalid inputs" $ do
      shouldFail ""
      shouldFail "+"
      shouldFail "-a"
      shouldFail "0x"
      shouldFail "0.a"
      shouldFail "--20"
      shouldFail "-+20"
      shouldFail "++20"
      shouldFail "+-20"
    it "parses positive integers" $ do
      "0" ==> 0
      "123" ==> 123
      "93642" ==> 93642
    it "parses integers with a sign" $ do
      "+1" ==> 1
      "-1" ==> (-1)
      "+0" ==> 0
      "-42" ==> (-42)
      "+500" ==> 500
    it "parses hex integers" $ do
      "0x0" ==> 0
      "+0xff" ==> 255
      "-0x15" ==> (-21)
    it "parses numbers with decimals" $ do
      "123." ==> 123
      "-9.81" ==> (-9.81)
      "+5e4" ==> 50000
      "2.5e-3" ==> 0.0025
  describe "stringP" $ do
    let shouldFail input =
          parse (stringP <* eof) "" input `shouldSatisfy` isLeft
    let input ==> output =
          parse (stringP <* eof) "" input `shouldBe` Right (LispString output)
    it "fails on invalid inputs" $ do
      shouldFail "no quotes at all"
      shouldFail "\""
      shouldFail "\"\n\""
      shouldFail "\"no closing quotation mark"
      shouldFail "no opening quotation mark\""
      shouldFail "\"\"\""
    it "parses string literals" $ do
      "\"this is some text\"" ==> "this is some text"
      "\"''\"" ==> "''"
      "\"abc123!#$%\"" ==> "abc123!#$%"
      "\"backslash \\ doesn't escape\"" ==> "backslash \\ doesn't escape"
  describe "symP" $ do
    let shouldFail input = parse (symP <* eof) "" input `shouldSatisfy` isLeft
    let input ==> output =
          parse (symP <* eof) "" input `shouldBe` Right (LispSym output)
    it "rejects invalid symbols" $ do
      shouldFail ""
      shouldFail "123"
    it "parses valid symbols" $ do
      "playerX" ==> "playerX"
      "var1" ==> "var1"
      "camelCase" ==> "camelCase"
      "PascalCase" ==> "PascalCase"
      "snake_case" ==> "snake_case"
      "SCREAMING-KEBAB-CASE" ==> "SCREAMING-KEBAB-CASE"
      "+" ==> "+"
      ">>=" ==> ">>="
      "^_^" ==> "^_^"
  describe "nodeP" $ do
    let shouldFail input = parse (nodeP <* eof) "" input `shouldSatisfy` isLeft
    let input ==> output = parse (nodeP <* eof) "" input `shouldBe` Right output
    it "fails on invalid inputs" $ do
      shouldFail ""
      shouldFail "("
      shouldFail "(foo"
      shouldFail "bar)"
    it "parses simple node expressions" $ do
      "(+ 1 2e3)" ==> LispNode (LispSym "+") [LispNum 1, LispNum 2000]
      "(zero? 0)" ==> LispNode (LispSym "zero?") [LispNum 0]
    it "parses nested nodes" $ do
      "(> (* 2 3) (+ 4 5))" ==>
        LispNode
          (LispSym ">")
          [ LispNode (LispSym "*") [LispNum 2, LispNum 3]
          , LispNode (LispSym "+") [LispNum 4, LispNum 5]
          ]
      "((foo) (bar))" ==>
        LispNode (LispNode (LispSym "foo") []) [LispNode (LispSym "bar") []]
