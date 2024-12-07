{-# LANGUAGE OverloadedStrings #-}

module CardGameParser
    ( parseGameFile ) -- Exported function for Main.hs
    where

import Data.Aeson (Value, object, (.=), encode)
import qualified Data.ByteString.Lazy as B
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)

-- Data Types
data Card = Card { name :: String, value :: Int, color :: String } deriving Show
data Rule = Rule { ruleName :: String, winner :: String } deriving Show
data Option = Option { optName :: String, optValue :: String } deriving Show
data Action = Action { actionName :: String, trigger :: String } deriving Show

-- Card Parser
cardParser :: Parser Card
cardParser = do
    spaces
    void $ string "card "
    cardName <- quotedString
    spaces
    void $ string "value "
    cardValue <- read <$> many1 digit
    spaces
    void $ string "color "
    cardColor <- many1 letter
    spaces
    return $ Card cardName cardValue cardColor

-- Rule Parser
ruleParser :: Parser Rule
ruleParser = do
    spaces
    void $ string "rule "
    rName <- quotedString
    spaces
    void $ string "winner "
    winnerName <- quotedString
    spaces
    return $ Rule rName winnerName

-- Option Parser
optionParser :: Parser Option
optionParser = do
    spaces
    void $ string "option "
    optName <- quotedString
    spaces
    optValue <- quotedString
    spaces
    return $ Option optName optValue

-- Action Parser
actionParser :: Parser Action
actionParser = do
    spaces
    void $ string "action "
    actionName <- quotedString
    spaces
    void $ string "each turn"
    spaces
    return $ Action actionName "each turn"

-- Metadata Parser
metadataParser :: Parser (String, Int, Int)
metadataParser = do
    spaces
    void $ string "game "
    gName <- quotedString
    spaces
    void $ string "players "
    pCount <- read <$> many1 digit
    spaces
    void $ string "rounds "
    rCount <- read <$> many1 digit
    spaces
    return (gName, pCount, rCount)

-- Quoted String Parser
quotedString :: Parser String
quotedString = char '"' >> manyTill anyChar (char '"')

-- Main Game Parser
gameParser :: Parser (String, Int, Int, [Card], [Rule], [Option], [Action])
gameParser = do
    (gName, pCount, rCount) <- metadataParser
    cards <- many (cardParser <* spaces)
    rules <- many (ruleParser <* spaces)
    options <- many (optionParser <* spaces)
    actions <- many (actionParser <* spaces)
    return (gName, pCount, rCount, cards, rules, options, actions)

-- Convert Parsed Data to JSON
generateJSON :: String -> Int -> Int -> [Card] -> [Rule] -> [Option] -> [Action] -> IO ()
generateJSON gName pCount rCount cards rules options actions = do
    let json = object
            [ "gameName" .= gName
            , "players" .= pCount
            , "rounds" .= rCount
            , "cards" .= map cardToJSON cards
            , "rules" .= map ruleToJSON rules
            , "options" .= map optionToJSON options
            , "actions" .= map actionToJSON actions
            ]
    B.writeFile "game.json" (encode json)

cardToJSON :: Card -> Value
cardToJSON (Card n v c) = object ["name" .= n, "value" .= v, "color" .= c]

ruleToJSON :: Rule -> Value
ruleToJSON (Rule n w) = object ["name" .= n, "winner" .= w]

optionToJSON :: Option -> Value
optionToJSON (Option n v) = object ["name" .= n, "value" .= v]

actionToJSON :: Action -> Value
actionToJSON (Action n t) = object ["name" .= n, "trigger" .= t]

-- Parse Game File with Error Handling
parseGameFile :: FilePath -> IO ()
parseGameFile filePath = do
    input <- readFile filePath
    case parse gameParser "" input of
        Left err -> do
            putStrLn "Error while parsing the file:"
            print err
        Right (gName, pCount, rCount, cards, rules, options, actions) -> do
            putStrLn "Parsing completed successfully."
            putStrLn $ "Game Name: " ++ gName
            putStrLn $ "Players: " ++ show pCount
            putStrLn $ "Rounds: " ++ show rCount
            putStrLn "Cards:"
            mapM_ print cards
            putStrLn "Rules:"
            mapM_ print rules
            putStrLn "Options:"
            mapM_ print options
            putStrLn "Actions:"
            mapM_ print actions
            generateJSON gName pCount rCount cards rules options actions
            putStrLn "Game configuration saved to game.json"
