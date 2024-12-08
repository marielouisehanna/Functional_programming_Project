{-# LANGUAGE OverloadedStrings #-}

import CardGameParser (parseGameFile, writeGameDataToJson)
import Text.Parsec (parse)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile, outputFile] -> do
            content <- readFile inputFile
            case parse parseGameFile inputFile content of
                Left err -> putStrLn $ "Error parsing game file: " ++ show err
                Right gameData -> writeGameDataToJson gameData outputFile
        _ -> putStrLn "Usage: CardGameParser <input file> <output file>"
