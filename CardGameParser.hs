{-# LANGUAGE OverloadedStrings #-}

module CardGameParser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)

-- Data types for the game
data Card = Card { name :: String, stats :: [String] } deriving (Show, Generic)
data Definition = Definition { name :: String, args :: [String] } | Extension { name :: String, original :: String } deriving (Show, Generic)
data Action = Action { function :: String, trigger :: String } deriving (Show, Generic)
data Game = Game
  { gameName :: String
  , numPlayers :: Int
  , cards :: [Card]
  , definitions :: [Definition]
  , actions :: [Action]
  } deriving (Show, Generic)

instance ToJSON Card
instance ToJSON Rule
instance ToJSON Action
instance ToJSON Game

-- Parsers
gameParser :: Parser String
gameParser = do
  string "game" >> spaces
  char '"'
  gName <- manyTill anyChar (char '"')
  spaces
  return gName

playersParser :: Parser Int
playersParser = do
  string "players" >> spaces
  pCount <- read <$> many1 digit
  spaces
  return pCount

cardParser :: Parser Card
cardParser = do
  string "card" >> spaces
  char '"'
  cName <- manyTill anyChar (char '"')
  spaces
  string "value" >> spaces
  cValue <- read <$> many1 digit
  spaces
  string "color" >> spaces
  cColor <- choice [char '"' >> manyTill anyChar (char '"'), many1 letter]
  spaces
  return $ Card cName cValue cColor


definitionParser :: Parser Definition
definitionParser = do
  string "define" >> spaces
  char '"'
  typ <- manyTill anyChar (char '"')
  spaces
  string "winner" >> spaces
  char '"'
  description <- manyTill anyChar (char '"')
  spaces
  return $ Definition typ description

actionParser :: Parser Action
actionParser = do
  string "action" >> spaces
  char '"'
  aName <- manyTill anyChar (char '"')
  spaces
  return $ Action aName

gameFileParser :: Parser Game
gameFileParser = do
  gName <- gameParser
  pCount <- playersParser
  cardList <- many cardParser
  definitionList <- many definitionParser
  actionList <- many actionParser
  return $ Game gName pCount cardList definitionList actionList

-- Generate card images
{--
generateCardImage :: Card -> IO ()
generateCardImage (Card name value color) = do
  let width = 200
      height = 300
      backgroundColor = case color of
        "red" -> PixelRGB8 255 0 0
        "black" -> PixelRGB8 0 0 0
        "blue" -> PixelRGB8 0 0 255
        _ -> PixelRGB8 255 255 255  -- Default to white
      fontColor = PixelRGB8 255 255 255  -- White font color for contrast
      image = generateImage (\x y ->
          if y > 50 && y < 100 && x > 50 && x < 150
              then fontColor else backgroundColor) width height
  writePng ("cards/" ++ name ++ ".png") image
-}
-- Parse and write JSON and generate card images
parseGameFile :: FilePath -> IO ()
parseGameFile file = do
  content <- readFile file
  case parse gameFileParser "" content of
    Left err -> do
      putStrLn "Error while parsing the file:"
      print err
    Right game -> do
      B.writeFile "C:/Users/user/Desktop/cards/game.json" (encode game)
      -- mapM_ generateCardImage (cards game)
      putStrLn "Game parsed and saved to game.json"
