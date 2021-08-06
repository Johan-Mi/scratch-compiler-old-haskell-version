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
  , eof
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
import Text.Printf (printf)

ws :: Parser ()
ws = spaces

programP :: Parser [LispAST]
programP = ws *> many (exprP <* ws) <* eof

exprP :: Parser LispAST
exprP = choice [try numP, stringP, symP, nodeP, unquoteP]

numP :: Parser LispAST
numP = LispNum <$> pm (read <$> choice [hexInt, expDec, dec, int])
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

unquoteP :: Parser LispAST
unquoteP = LispUnquote <$> (char ',' *> ws *> exprP)
