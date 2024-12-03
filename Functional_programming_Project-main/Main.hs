module Main where

import CardGameParser (parseGameFile)

main :: IO ()
main = do
    putStrLn "Processing game.txt with CardGameParser..."
    parseGameFile "game.txt"
    putStrLn "Done! Check game.json for output."
