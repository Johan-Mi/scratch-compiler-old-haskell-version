module Parser
  ( programP
  , numP
  , numberP
  , stringP
  , symP
  , nodeP
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import qualified Data.Text as T
import LispAST (LispAST(..))
import Text.Parsec
  ( char
  , choice
  , digit
  , eof
  , hexDigit
  , letter
  , many
  , many1
  , noneOf
  , oneOf
  , skipMany
  , space
  , try
  )
import Text.Parsec.Text (Parser)
import Text.Printf (printf)

comment :: Parser ()
comment = char ';' *> skipMany (noneOf "\n") *> (void (char '\n') <|> eof)

ws :: Parser ()
ws = skipMany $ void space <|> comment

programP :: Parser [LispAST]
programP = ws *> many (exprP <* ws) <* eof

exprP :: Parser LispAST
exprP = choice [try numP, stringP, symP, nodeP, unquoteP]

-- TODO: ECMAScript-compliant number parser
--
-- StrNumericLiteral :::
--   StrDecimalLiteral
--   NonDecimalIntegerLiteral
-- StrDecimalLiteral :::
--   StrUnsignedDecimalLiteral
--   + StrUnsignedDecimalLiteral
--   - StrUnsignedDecimalLiteral
-- StrUnsignedDecimalLiteral :::
--   Infinity
--   DecimalDigits? . DecimalDigits[~Sep]opt ExponentPart?
--   DecimalDigits ExponentPart[~Sep]opt
numberP :: Parser Double
numberP = pm (read <$> choice [hexInt, expDec, dec, int])
  where
    positive = (char '+' *>)
    negative :: Parser Double -> Parser Double
    negative p = negate <$> (char '-' *> p)
    pm p = p <|> positive p <|> negative p
    int = many1 digit
    negInt = (:) <$> char '-' <*> int
    hexInt = try $ ("0x" ++) <$> (char '0' *> oneOf "xX" *> many1 hexDigit)
    dec = try $ printf "%s.%s0" <$> (many1 digit <* char '.') <*> many digit
    decOrInt = dec <|> int
    expDec =
      try $ printf "%se%s" <$> (decOrInt <* oneOf "eE") <*> (int <|> negInt)

numP :: Parser LispAST
numP = LispNum <$> numberP

stringP :: Parser LispAST
stringP = LispString . T.pack <$> (char '"' *> stringContent <* char '"')
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

unquoteP :: Parser LispAST
unquoteP = LispUnquote <$> (char ',' *> ws *> exprP)
