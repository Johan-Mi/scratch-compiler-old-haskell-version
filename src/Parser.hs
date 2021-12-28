module Parser
  ( programP
  , numP
  , numberP
  , stringP
  , symP
  , nodeP
  ) where

import Control.Applicative ((<|>), liftA2)
import Control.Monad (void)
import qualified Data.Text as T
import LispAST (LispAST(..))
import Text.Parsec
  ( (<?>)
  , between
  , char
  , choice
  , digit
  , eof
  , hexDigit
  , letter
  , many
  , many1
  , noneOf
  , oneOf
  , optional
  , skipMany
  , space
  , try
  )
import Text.Parsec.Text (Parser)

(<:>) :: Parser a -> Parser [a] -> Parser [a]
(<:>) = liftA2 (:)

infixr <:>

comment :: Parser ()
comment =
  char ';' *> skipMany (noneOf "\n") *> (void (char '\n') <|> eof) <?> "comment"

ws :: Parser ()
ws = skipMany $ void space <|> comment

programP :: Parser [LispAST]
programP = ws *> many (exprP <* ws) <* eof

exprP :: Parser LispAST
exprP = choice [try numP, stringP, symP, nodeP, unquoteP] <?> "expression"

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
numberP = read <$> pm (choice [hexInt, expDec, dec, int])
  where
    pm p = (char '-' <:> p) <|> (optional (char '+') *> p)
    int = many1 digit
    negInt = char '-' <:> int
    hexInt = try $ char '0' <:> oneOf "xX" <:> many1 hexDigit
    dec = try $ many1 digit <> (char '.' <:> ((<> "0") <$> many digit))
    decOrInt = dec <|> int
    expDec = try $ decOrInt <> (oneOf "eE" <:> (int <|> negInt))

numP :: Parser LispAST
numP = LispNum <$> numberP <?> "number"

stringP :: Parser LispAST
stringP =
  LispString . T.pack <$>
  (char '"' *> stringContent <* char '"') <?> "string literal"
  where
    stringContent = many $ (char '\\' *> escaped) <|> noneOf "\n\"\\"
    escaped = oneOf "\"\\"

symP :: Parser LispAST
symP = LispSym . T.pack <$> (firstChar <:> many nonFirstChar) <?> "symbol"
  where
    firstChar = letter <|> oneOf "!$%&*+-./:<=>?@^_~[]"
    nonFirstChar = firstChar <|> digit

inParens :: Parser a -> Parser a
inParens = between (char '(' *> ws) (ws <* char ')')

nodeP :: Parser LispAST
nodeP = inParens (LispNode <$> exprP <*> many (ws *> exprP)) <?> "node"

unquoteP :: Parser LispAST
unquoteP = LispUnquote <$> (char ',' *> ws *> exprP) <?> "unquote"
