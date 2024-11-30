module Main where

import CardGameParser (parseGameFile)

main :: IO ()
main = do
  putStrLn "Processing game.txt to generate game.json and card images..."
  parseGameFile "game.txt"
  putStrLn "Done! Check game.json and the cards/ directory for output."
