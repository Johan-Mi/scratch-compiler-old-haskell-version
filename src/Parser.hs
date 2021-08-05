module Parser
  ( programP
  , numP
  , stringP
  , symP
  , nodeP
  ) where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import LispAST (LispAST(..))
import Text.Parsec
  ( char
  , choice
  , digit
  , hexDigit
  , letter
  , many
  , many1
  , noneOf
  , oneOf
  , spaces
  , try
  )
import Text.Parsec.Text (Parser)

ws :: Parser ()
ws = spaces

programP :: Parser [LispAST]
programP = ws *> many (exprP <* ws)

exprP :: Parser LispAST
exprP = choice [try numP, stringP, symP, nodeP]

numP :: Parser LispAST
numP = LispNum <$> pm (read <$> choice [hexInt, expDec, dec, int])
  where
    positive = (char '+' *>)
    negative :: Parser Double -> Parser Double
    negative p = negate <$> (char '-' *> p)
    pm p = p <|> positive p <|> negative p
    int = many1 digit
    negInt = (:) <$> char '-' <*> int
    hexInt =
      try $ do
        char '0'
        oneOf "xX"
        digits <- many1 hexDigit
        return $ "0x" ++ digits
    dec =
      try $ do
        intPart <- many1 digit
        char '.'
        fracPart <- many digit
        return $ intPart ++ "." ++ fracPart ++ "0"
    decOrInt = dec <|> int
    expDec =
      try $ do
        factor <- decOrInt
        oneOf "eE"
        exponent <- int <|> negInt
        return $ factor ++ "e" ++ exponent

stringP :: Parser LispAST
stringP = (LispString . T.pack) <$> (char '"' *> stringContent <* char '"')
  where
    stringContent = many $ noneOf "\n\""

symP :: Parser LispAST
symP = LispSym . T.pack <$> ((:) <$> firstChar <*> many nonFirstChar)
  where
    firstChar = letter <|> oneOf "!$%&*+-./:<=>?@^_~"
    nonFirstChar = firstChar <|> digit

inParens :: Parser a -> Parser a
inParens p = char '(' *> ws *> p <* ws <* char ')'

nodeP :: Parser LispAST
nodeP = inParens $ LispNode <$> exprP <*> many (ws *> exprP)
