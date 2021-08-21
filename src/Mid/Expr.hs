{-# LANGUAGE OverloadedStrings #-}

module Mid.Expr
  ( Expr(..)
  , Value(..)
  , getLit
  , mkExpr
  , subExprs
  , toString
  , toNum
  , toBool
  ) where

import Data.Char (intToDigit)
import Data.Either (fromRight)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Builder as B
import Lens.Micro (Traversal')
import LispAST (LispAST(..))
import Mid.Error (MidError(..))
import Numeric (floatToDigits)
import Parser (numberP)
import Text.Parsec (eof, parse, spaces)

data Value
  = VNum Double
  | VStr T.Text
  | VBool Bool
  deriving (Show)

data Expr
  = Lit Value
  | Sym T.Text
  | FuncCall T.Text [Expr]
  deriving (Show)

getLit :: Expr -> Maybe Value
getLit (Lit v) = Just v
getLit _ = Nothing

mkExpr :: LispAST -> Either MidError Expr
mkExpr (LispNum n) = Right $ Lit $ VNum n
mkExpr (LispString s) = Right $ Lit $ VStr s
mkExpr (LispSym s) = Right $ Sym s
mkExpr (LispNode (LispSym name) args) = FuncCall name <$> traverse mkExpr args
mkExpr ast = Left $ NotAnExpression ast

subExprs :: Traversal' Expr Expr
subExprs _ (Lit v) = pure $ Lit v
subExprs _ (Sym t) = pure $ Sym t
subExprs f (FuncCall name exprs) = FuncCall name <$> traverse f exprs

toString :: Value -> T.Text
toString (VStr txt) = txt
toString (VNum 0) = "0"
toString (VNum num')
  | isNaN num' = "NaN"
  | isInfinite num' = "Infinity"
  | otherwise = toStrict $ B.toLazyText $ go num'
  where
    go num
      | num < 0 = B.singleton '-' <> go (-num)
      | k <= n && n <= 21 =
        B.fromString dstr <> B.fromText (T.replicate power "0")
      | 0 < n && n <= 21 =
        let (before, after) = splitAt n dstr
         in B.fromString before <> B.singleton '.' <> B.fromString after
      | (-6) < n && n <= 0 =
        B.fromText "0." <>
        B.fromText (T.replicate (-n) "0") <> B.fromString dstr
      | k == 1 = B.fromString dstr <> middle <> B.fromString (show $ n - 1)
      | otherwise =
        let (d:ds) = dstr
         in B.singleton d <>
            B.singleton '.' <>
            B.fromString ds <> middle <> B.fromString (show $ n - 1)
      where
        (digits, n) = floatToDigits 10 num
        dstr = intToDigit <$> digits
        k = length digits
        power = n - k
        middle =
          if n > 0
            then B.fromText "e+"
            else B.singleton 'e'
toString (VBool True) = "true"
toString (VBool False) = "false"

toNum :: Value -> Double
toNum (VStr str) =
  fromRight (0 / 0) $ parse (spaces *> numberP <* spaces <* eof) "" str
toNum (VNum num) = num
toNum (VBool True) = 1
toNum (VBool False) = 0

toBool :: Value -> Bool
toBool (VStr str) = T.toLower str `notElem` ["", "0", "false"]
toBool (VNum num) = num /= 0 && not (isNaN num)
toBool (VBool b) = b
