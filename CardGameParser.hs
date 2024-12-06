{-# LANGUAGE OverloadedStrings #-}

module CardGameParser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)
import Data.List.Split

-- Data types for the game
--data Card = Card { name :: String, stats :: [String] } deriving (Show, Generic)
--data Definition = Definition { name :: String, args :: [String] } | Extension { name :: String, original :: String } deriving (Show, Generic)
--data Action = Action { function :: String, trigger :: String } deriving (Show, Generic)
--data Game = Game
--  { gameName :: String
--  , numPlayers :: Int
--  , cards :: [Card]
--  , definitions :: [Definition]
--  , actions :: [Action]
--  } deriving (Show, Generic)

-- 2nd try    !! later switch all Ints with Exprs to allow arithmetics
--data Program = Program { setup :: Setup, loop :: Loop } deriving (Show, Generic)
--data Setup = Setup { code :: [Line] } deriving (Show, Generic)
--data Loop = Loop { code :: [Line] } deriving (Show, Generic)
data Program = Program [CodeLine] deriving (Show, Generic)
data CodeLine = SettingLine Setting
          | TypeLine Type
          | CreationLine Creation
          | DefinitionLine Definition
          | TriggerLine Trigger deriving (Show, Generic)
data Setting = Setting Option deriving (Show, Generic)
data Option = GameName String
            | Players Int
            | WindowSize Size
            | CardSize Size
            | ImageSize Size deriving (Show, Generic)
data Size = Size { width :: Int, height :: Int } deriving (Show, Generic)
data Type = Type { typeName :: String, attributes :: [String] } deriving (Show, Generic)
data Creation = Creation { cardName :: String, typed :: Type, values :: [String] } deriving (Show, Generic)
data Definition = Definition { funcName :: String, types :: [Type], returnType :: Return} deriving (Show, Generic)
data Return = ReturnType Type | ReturnFunction Definition [String] deriving (Show, Generic)
data Trigger = Trigger { name :: Definition, args :: [String], action :: Action, givenValues :: [String] } deriving (Show, Generic)
data Action = DefinedAction PreDefined | FunctionAction Definition deriving (Show, Generic)
data PreDefined = Click | Hover deriving (Show, Generic)

-- still have to define parsers

instance ToJSON Program
instance ToJSON CodeLine
instance ToJSON Setting
instance ToJSON Option
instance ToJSON Size
instance ToJSON Type
instance ToJSON Creation
instance ToJSON Definition
instance ToJSON Return
instance ToJSON Trigger
instance ToJSON Action
instance ToJSON PreDefined


sizeParser :: Parser Size
sizeParser = do
  width <- read <$> many1 digit
  spaces
  char 'x'
  spaces
  height <- read <$> many1 digit
  spaces
  return $ Size width height

optionParser :: Parser Option
optionParser = do
  string "game-name" >> spaces
  char '"'
  gameName <- manyTill anyChar (char '"')
  spaces
  return $ GameName gameName
  <|> do
  string "players" >> spaces
  playerCount <- read <$> many1 digit
  return $ Players playerCount
  <|> do
  string "window-size" >> spaces
  char '='
  spaces
  size <- sizeParser
  spaces
  return $ WindowSize size
  <|> do
  string "card-default-size" >> spaces
  char '='
  spaces
  size <- sizeParser
  spaces
  return $ CardSize size
  <|> do
  string "image-default-size" >> spaces
  char '='
  spaces
  size <- sizeParser
  spaces
  return $ ImageSize size

settingParser :: Parser Setting
settingParser = do
  string "set" >> spaces
  option <- optionParser
  spaces
  return $ Setting option

listParser :: Parser [String]
listParser = do
  char '[' >> spaces
  list <- manyTill anyChar (char ']')
  spaces
  return $ splitOn "," list


typeParser :: Parser Type
typeParser = do
  string "type card" >> spaces
  char '"'
  typeName <- manyTill anyChar (char '"')
  spaces
  attributes <- listParser
  spaces
  return $ Type typeName attributes

--creationParser :: Parser Creation
--creationParser = do
--  string "card" >> spaces
--  char '"'
--  cardName <- manyTill anyChar (char '"')
--  spaces
--  ?????
--  return Creation cardName ...

--definitionParser :: Parser Definition
--definitionParser = do
--  string "define" >> spaces
--  char '"'
--  functionName <- manyTill anyChar (char '"')
--  spaces
--  string "takes"
--  spaces
--  ??????
--  return Definition functionName ...

-- probably, no very likely, not necessary, should be changed
--actionParser :: Parser Action
--actionParser = do
--  char '"' >> spaces
--  ???
--  spaces
--  return FunctionAction ???
--  <|>
--  preDefined <- preDefinedParser

--triggerParser :: Parser Trigger
--triggerParser = do
--  string "trigger" >> spaces
--  function <- ???
--  spaces
--  string "on"
--  spaces
--  action <- actionParser
--  spaces
--  ????

lineParser :: Parser CodeLine
lineParser = do
  line <- settingParser
  return $ SettingLine line
  <|> do
  line <- typeParser
  return $ TypeLine line
--  <|> do
--  line <- creationParser
--  return CreationLine line
--  <|> do
--  line <- definitionParser
--  return DefinitionLine line
--  <|> do
--  line <- triggerParser
--  return TriggerLine line

--setupParser :: Parser Setup
--setupParser = do
--
--loopParser :: Parser Loop
--loopParser = do

programParser :: Parser Program
programParser = do
  code <- many lineParser
  return $ Program code

parseGameFile :: FilePath -> IO ()
parseGameFile file = do
  content <- readFile file
  case parse programParser "" content of
    Left err -> do
      putStrLn "Error while parsing the file:"
      print err
    Right game -> do
      B.writeFile "C:/Users/user/Desktop/cards/game.json" (encode game)
      -- mapM_ generateCardImage (cards game)
      putStrLn "Game parsed and saved to game.json"
