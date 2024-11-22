module Main where

import GameParser
import GameEngine
import Parsing
import System.IO (readFile)

main :: IO ()
main = do
  script <- readFile "game.txt"
  case runParser gameParser script of
    [(scenes, "")] ->
      case findScene "start" scenes of
        Just startScene -> playScene startScene scenes
        Nothing -> putStrLn "No start scene found!"
    _ -> putStrLn "Failed to parse the game script."
