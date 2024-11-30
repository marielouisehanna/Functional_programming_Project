-- CardGameParser.hs
{-# LANGUAGE OverloadedStrings #-}

module CardGameParser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)

-- Data types for the game
data Card = Card { name :: String, value :: Int } deriving (Show, Generic)
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
gameParser = string "game" >> spaces >> char '"' >> manyTill anyChar (char '"') <* spaces

playersParser :: Parser Int
playersParser = string "players" >> spaces >> read <$> many1 digit <* spaces


cardParser :: Parser Card
cardParser = do
    string "card" >> spaces
    char '"'
    name <- manyTill anyChar (char '"')
    spaces
    string "value" >> spaces
    value <- read <$> many1 digit
    spaces
    return $ Card name value

ruleParser :: Parser Rule
ruleParser = do
    string "rule" >> spaces
    char '"'
    name <- manyTill anyChar (char '"')
    spaces
    string "winner" >> spaces
    char '"'
    condition <- manyTill anyChar (char '"')
    spaces
    return $ Rule name condition

actionParser :: Parser Action
actionParser = do
    string "action" >> spaces
    char '"'
    name <- manyTill anyChar (char '"')
    spaces
    string "each" >> spaces
    trigger <- many1 letter
    spaces
    return $ Action name trigger


gameFileParser :: Parser Game
gameFileParser = do
  name <- gameParser
  players <- playersParser
  cardList <- many cardParser
  ruleList <- many ruleParser
  actionList <- many actionParser
  return $ Game name players cardList ruleList actionList

-- Parse and write JSON
parseGameFile :: FilePath -> IO ()
parseGameFile file = do
  content <- readFile file
  case parse gameFileParser "" content of
    Left err -> print err
    Right game -> B.writeFile "game.json" (encode game)
