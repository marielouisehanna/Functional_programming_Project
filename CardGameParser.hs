{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CardGameParser (
    GameData(..),
    parseGameFile,
    writeGameDataToJson
) where

import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Data.ByteString.Lazy as B

-- Data Types
data Game = Game {
    gameName :: String
} deriving (Show, Generic)
instance ToJSON Game

data Player = Player {
    playerName     :: String,
    playerStrategy :: String
} deriving (Show, Generic)
instance ToJSON Player

data Card = Card {
    cardName  :: String,
    cardValue :: Int,
    cardImage :: Maybe String
} deriving (Show, Generic)
instance ToJSON Card

data Rule = Rule {
    ruleName   :: String,
    ruleDetail :: String
} deriving (Show, Generic)
instance ToJSON Rule

data GameData = GameData {
    game       :: Maybe Game,
    players    :: [Player],
    rounds     :: Maybe Int,
    cards      :: [Card],
    rules      :: [Rule]
} deriving (Show, Generic)
instance ToJSON GameData

-- Parsers
parseGame :: Parser Game
parseGame = do
    string "game"
    spaces
    name <- between (char '"') (char '"') (many (noneOf "\""))
    return $ Game name

parseRounds :: Parser Int
parseRounds = do
    string "rounds"
    spaces
    read <$> many1 digit

parsePlayer :: Parser Player
parsePlayer = do
    string "player"
    spaces
    name <- between (char '"') (char '"') (many (noneOf "\""))
    spaces
    string "strategy"
    spaces
    strategy <- between (char '"') (char '"') (many (noneOf "\""))
    return $ Player name strategy

parseCard :: Parser Card
parseCard = do
    string "card"
    spaces
    name <- between (char '"') (char '"') (many (noneOf "\""))
    spaces
    string "value"
    spaces
    value <- read <$> many1 digit
    image <- optionMaybe (try $ spaces >> string "image" >> spaces >> between (char '"') (char '"') (many (noneOf "\"")))
    return $ Card name value image

parseRule :: Parser Rule
parseRule = do
    string "rule"
    spaces
    name <- between (char '"') (char '"') (many (noneOf "\""))
    spaces
    detail <- manyTill anyChar (try (newline >> return '\n') <|> (eof >> return '\n'))
    return $ Rule name (unwords $ words detail)

-- Combine parsers
parseGameFile :: Parser GameData
parseGameFile = do
    g <- optionMaybe parseGame
    r <- optionMaybe parseRounds
    cs <- many (try parseCard)
    ps <- many (try parsePlayer)
    rs <- many (try parseRule)
    return $ GameData g ps r cs rs

-- Write GameData to JSON
writeGameDataToJson :: GameData -> FilePath -> IO ()
writeGameDataToJson gameData filePath = do
    let jsonData = encode gameData
    B.writeFile filePath jsonData
