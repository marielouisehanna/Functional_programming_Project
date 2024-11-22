
module ExprParser (
  Expr(..),
  expr,
  evaluate
) where

import Parsing
import Token
import Control.Applicative ( Alternative((<|>)) )

-- Define the expression data type
data Expr
  = Val Int       -- A value
  | Add Expr Expr -- Addition
  | Mul Expr Expr -- Multiplication
  deriving (Show, Eq)

-- Evaluate an expression
evaluate :: Expr -> Int
evaluate (Val n)       = n
evaluate (Add e1 e2)   = evaluate e1 + evaluate e2
evaluate (Mul e1 e2)   = evaluate e1 * evaluate e2

-- Parse a value (a natural number)
value :: Parser Expr
value = do
  n <- natural
  return (Val n)

-- Parse a factor (a value or a parenthesized expression)
factor :: Parser Expr
factor = value <|> do
  _ <- symbol "("
  e <- expr
  _ <- symbol ")"
  return e

-- Helper: Left-associative binary operation parser
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = (do
      f <- op
      y <- p
      rest (f x y)) <|> return x

-- Parse multiplication
term :: Parser Expr
term = chainl1 factor mulOp
  where
    mulOp = do
      _ <- symbol "*"
      return Mul

-- Parse addition
expr :: Parser Expr
expr = chainl1 term addOp
  where
    addOp = do
      _ <- symbol "+"
      return Add
