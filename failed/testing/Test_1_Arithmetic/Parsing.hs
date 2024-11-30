module Parsing (
  Parser(..),
  item,
  char,
  satisfy,
  string,
  space,
  token,
  many,
  some
) where

import Control.Applicative
import Data.Char

-- Parser type definition
newtype Parser a = Parser { runParser :: String -> [(a, String)] }

-- Functor instance for Parser
instance Functor Parser where
  fmap f (Parser p) = Parser $ \inp -> [(f v, out) | (v, out) <- p inp]

-- Applicative instance for Parser
instance Applicative Parser where
  pure v = Parser $ \inp -> [(v, inp)]
  (Parser pf) <*> (Parser pv) = Parser $ \inp -> 
    [(f v, out2) | (f, out1) <- pf inp, (v, out2) <- pv out1]

-- Monad instance for Parser
instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser $ \inp -> 
    concat [runParser (f v) out | (v, out) <- p inp]

-- Alternative instance for Parser
instance Alternative Parser where
  empty = Parser $ const []
  (Parser p) <|> (Parser q) = Parser $ \inp -> 
    case p inp of
      [] -> q inp
      res -> res

-- Basic parsers
item :: Parser Char
item = Parser $ \inp -> case inp of
  []     -> []
  (x:xs) -> [(x, xs)]

-- Parse a specific character
char :: Char -> Parser Char
char c = satisfy (== c)

-- Parse a character satisfying a predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  x <- item
  if p x
    then return x
    else empty

-- Parse a specific string
string :: String -> Parser String
string [] = return []
string (x:xs) = do
  _ <- char x
  _ <- string xs
  return (x:xs)

-- Parse and skip whitespace
space :: Parser ()
space = do
  _ <- many (satisfy isSpace)
  return ()

-- Token parser: ignores surrounding spaces
token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v
