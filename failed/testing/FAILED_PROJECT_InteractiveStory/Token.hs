module Token (
  digit,
  letter,
  identifier,
  symbol
) where

import Parsing
import Data.Char (isDigit, isAlpha)
import Control.Applicative
-- Parse a digit
digit :: Parser Char
digit = satisfy isDigit

-- Parse a letter
letter :: Parser Char
letter = satisfy isAlpha

-- Parse an identifier: starts with a letter, followed by letters or digits
identifier :: Parser String
identifier = token $ do
  first <- letter
  rest <- many (letter <|> digit)
  return (first : rest)

-- Parse a symbol (specific string)
symbol :: String -> Parser String
symbol s = token (string s)
