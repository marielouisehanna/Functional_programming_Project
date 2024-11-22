module Main where

import ExprParser
import Parsing

-- Evaluate an input string and handle errors
evaluateExpression :: String -> Either String Int
evaluateExpression input = case runParser expr input of
    [(result, "")] -> Right (evaluate result)
    _              -> Left "Invalid input!"

-- Main entry point
main :: IO ()
main = do
  putStrLn "Enter an expression:"
  input <- getLine
  case evaluateExpression input of
    Right value -> putStrLn $ "Result: " ++ show value
    Left err    -> putStrLn err
