module Main where

import CardGameParser
import qualified Data.ByteString.Lazy as B
import System.IO
import Text.Parsec

main :: IO ()
main = do
    putStrLn "Parsing the game.txt file..."
    input <- readFile "game.txt"
    case parse gameParser "" input of
        Left err -> print $ "Error: " ++ show err
        Right game -> do
            B.writeFile "game.json" (encode game)
            putStrLn "Parsing successful! game.json generated."
