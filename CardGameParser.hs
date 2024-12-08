{-# LANGUAGE OverloadedStrings #-}

module CardGameParser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Aeson (encode, ToJSON, toJSON, object, (.=))
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)

data Card = Card { name :: String, value :: Int, bg :: Maybe String, image :: String } deriving Show
data Rule = Rule String String deriving Show
data Game = Game String [String] [Card] [Rule] deriving Show

instance ToJSON Card where
    toJSON (Card name value bg image) = 
        object ["name" .= name, "value" .= value, "bg" .= bg, "image" .= image]

instance ToJSON Rule where
    toJSON (Rule key value) = object [key .= value]

instance ToJSON Game where
    toJSON (Game name players cards rules) =
        object ["name" .= name, "players" .= players, "cards" .= cards, "rules" .= rules]

-- Parser for cards
cardParser :: Parser Card
cardParser = do
    string "card"
    spaces
    cardName <- between (char '"') (char '"') (many1 (noneOf "\""))
    spaces
    string "value"
    spaces
    value <- read <$> many1 digit
    bg <- optionMaybe $ try (spaces *> string "bg" *> spaces *> between (char '"') (char '"') (many1 (noneOf "\"")))
    img <- optionMaybe $ try (spaces *> string "image" *> spaces *> between (char '"') (char '"') (many1 (noneOf "\"")))
    return $ Card cardName value bg (fromMaybe "default.png" img)

-- Parser for rules
ruleParser :: Parser Rule
ruleParser = do
    string "rule"
    spaces
    key <- between (char '"') (char '"') (many1 (noneOf "\""))
    spaces
    value <- between (char '"') (char '"') (many1 (noneOf "\""))
    return (Rule key value)

-- Full parser
gameParser :: Parser Game
gameParser = do
    string "game"
    spaces
    gameName <- between (char '"') (char '"') (many1 (noneOf "\""))
    spaces
    string "players"
    spaces
    players <- many1 (between (char '"') (char '"') (many1 (noneOf "\"")) <* spaces)
    cards <- many1 (try (spaces *> cardParser))
    rules <- many1 (try (spaces *> ruleParser))
    return $ Game gameName players cards rules

-- Main function
main :: IO ()
main = do
    input <- readFile "game.txt"
    case parse gameParser "" input of
        Left err -> print err
        Right game -> B.writeFile "game.json" (encode game)
