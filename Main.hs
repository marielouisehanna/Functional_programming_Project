module Main where

import ExprParser
import Parsing

-- Entry point
main :: IO ()
main = do
  putStrLn "Enter an expression:"
  input <- getLine
  case runParser expr input of
    [(result, "")] -> print result
    [(_, rest)]    -> putStrLn $ "Unconsumed input: " ++ rest
    []             -> putStrLn "Invalid expression!"
