module Token (
  digit,
  letter,
  natural,
  identifier,
  symbol
) where


import Parsing
import Data.Char
import Control.Applicative

-- Parse a digit
digit :: Parser Char
digit = satisfy isDigit

-- Parse a letter
letter :: Parser Char
letter = satisfy isAlpha

-- Parse a natural number
natural :: Parser Int
natural = token $ do
  digits <- some digit
  return (read digits)

-- Parse an identifier (letters followed by letters or digits)
identifier :: Parser String
identifier = token $ do
  first <- letter
  rest <- many (letter <|> digit)
  return (first : rest)

-- Parse a specific symbol (like "+", "(", etc.)
symbol :: String -> Parser String
symbol s = token (string s)
