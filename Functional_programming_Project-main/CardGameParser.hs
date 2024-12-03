{-# LANGUAGE OverloadedStrings #-}

module CardGameParser
    ( parseGameFile ) -- Fonction exportée pour Main.hs
    where

import Data.Aeson (Value, object, (.=), encode)
import qualified Data.ByteString.Lazy as B
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)

-- Types pour représenter les cartes, règles et options
data Card = Card { name :: String, value :: Int, color :: String, image :: Maybe String } deriving Show
data Rule = Rule { ruleName :: String, winner :: String, action :: Maybe String } deriving Show
data Option = Option { optName :: String, optValue :: String } deriving Show

-- Parser pour une carte
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
    cardImage <- optionMaybe (string "image " >> quotedString)
    spaces
    return $ Card cardName cardValue cardColor cardImage

-- Parser pour une règle
ruleParser :: Parser Rule
ruleParser = do
    spaces
    void $ string "rule "
    rName <- quotedString
    spaces
    void $ string "winner "
    winnerName <- quotedString
    spaces
    actionName <- optionMaybe (string "action " >> quotedString)
    spaces
    return $ Rule rName winnerName actionName

-- Parser pour une option
optionParser :: Parser Option
optionParser = do
    spaces
    void $ string "option "
    optName <- quotedString
    spaces
    optValue <- quotedString
    spaces
    return $ Option optName optValue

-- Parser pour des chaînes entre guillemets
quotedString :: Parser String
quotedString = char '"' >> manyTill anyChar (char '"')

-- Parser principal pour le fichier
gameParser :: Parser ([Card], [Rule], [Option])
gameParser = do
    spaces
    void $ string "game "
    quotedString
    spaces
    void $ string "players "
    many1 digit
    spaces
    cards <- many (cardParser <* spaces)
    rules <- many (ruleParser <* spaces)
    options <- many (optionParser <* spaces)
    return (cards, rules, options)

-- Convertit les données en JSON
generateJSON :: [Card] -> [Rule] -> [Option] -> IO ()
generateJSON cards rules options = do
    let json = object
            [ "cards" .= map cardToJSON cards
            , "rules" .= map ruleToJSON rules
            , "options" .= map optionToJSON options
            ]
    B.writeFile "game.json" (encode json)

cardToJSON :: Card -> Value
cardToJSON (Card n v c i) = object ["name" .= n, "value" .= v, "color" .= c, "image" .= i]

ruleToJSON :: Rule -> Value
ruleToJSON (Rule n w a) = object ["name" .= n, "winner" .= w, "action" .= a]

optionToJSON :: Option -> Value
optionToJSON (Option n v) = object ["name" .= n, "value" .= v]

-- Fonction principale pour traiter un fichier
parseGameFile :: FilePath -> IO ()
parseGameFile filePath = do
    input <- readFile filePath
    case parse gameParser "" input of
        Left err -> do
            putStrLn "Error while parsing the file:"
            print err
        Right (cards, rules, options) -> do
            print cards  -- Debug : Affiche les cartes détectées
            print rules  -- Debug : Affiche les règles détectées
            print options -- Debug : Affiche les options détectées
            generateJSON cards rules options
            putStrLn "Game parsed and saved to game.json"
