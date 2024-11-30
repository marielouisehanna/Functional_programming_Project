{-# LANGUAGE OverloadedStrings #-}

module CardGameParser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)
import Codec.Picture -- JuicyPixels for image creation
import Codec.Picture.Types (PixelRGB8(..), generateImage)

-- Data types for the game
data Card = Card { name :: String, value :: Int, color :: String } deriving (Show, Generic)
data Rule = Rule { ruleName :: String, condition :: String } deriving (Show, Generic)
data Action = Action { actionName :: String, trigger :: String } deriving (Show, Generic)
data Game = Game
  { gameName :: String
  , numPlayers :: Int
  , cards :: [Card]
  , rules :: [Rule]
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


ruleParser :: Parser Rule
ruleParser = do
  string "rule" >> spaces
  char '"'
  rName <- manyTill anyChar (char '"')
  spaces
  string "winner" >> spaces
  char '"'
  condition <- manyTill anyChar (char '"')
  spaces
  return $ Rule rName condition

actionParser :: Parser Action
actionParser = do
  string "action" >> spaces
  char '"'
  aName <- manyTill anyChar (char '"')
  spaces
  string "each" >> spaces
  trigger <- many1 letter
  spaces
  return $ Action aName trigger

gameFileParser :: Parser Game
gameFileParser = do
  gName <- gameParser
  pCount <- playersParser
  cardList <- many cardParser
  ruleList <- many ruleParser
  actionList <- many actionParser
  return $ Game gName pCount cardList ruleList actionList

-- Generate card images
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

-- Parse and write JSON and generate card images
parseGameFile :: FilePath -> IO ()
parseGameFile file = do
  content <- readFile file
  case parse gameFileParser "" content of
    Left err -> do
      putStrLn "Error while parsing the file:"
      print err
    Right game -> do
      B.writeFile "game.json" (encode game)
      mapM_ generateCardImage (cards game)
      putStrLn "Game parsed and saved to game.json"
