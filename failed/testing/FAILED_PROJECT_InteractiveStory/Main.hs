module Main where

import GameParser
import GameEngine
import Parsing
import System.IO (readFile)

main :: IO ()
main = do
  script <- readFile "game.txt"
  putStrLn "Script loaded. Attempting to parse..."
  case runParser gameParser script of
    [(scenes, "")] -> do
      putStrLn "Parsing succeeded!"
      case findScene "start" scenes of
        Just startScene -> playScene startScene scenes
        Nothing -> putStrLn "No start scene found!"
    _ -> do
      putStrLn "Failed to parse the game script."
      putStrLn "Partial parsing result:"
      print (runParser gameParser script)

