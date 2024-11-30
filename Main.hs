-- Main.hs
module Main where

import CardGameParser (parseGameFile)

main :: IO ()
main = do
    -- Define the input and output file paths
    let inputFile = "game.txt"
    let outputFile = "game.json"
    -- Parse the input file and write to the output JSON file
    parseGameFile inputFile
    putStrLn $ "Game parsed and saved to " ++ outputFile
