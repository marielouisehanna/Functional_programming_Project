{-# LANGUAGE OverloadedStrings #-}

module CardGameParser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)

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
data Program = Program { setup :: Setup, loop :: Loop } deriving (Show, Generic)
data Setup = Setup { code :: [Line] } deriving (Show, Generic)
data Loop = Loop { code :: [Line] } deriving (Show, Generic)
data Line = SettingLine Setting
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
data Type = Type { name :: String, attributes :: [String] } deriving (Show, Generic)
data Creation = Creation { name :: String, typed :: Type, values :: ??? } deriving (Show, Generic)
data Definition = Definition { name :: String, types :: [Type], return :: Return} deriving (Show, Generic)
data Return = ReturnType Type | ReturnFunction Definition [String] deriving (Show, Generic)
data Trigger = Trigger { name :: Definition, args :: [String], action :: Action, values :: [String] } deriving (Show, Generic)
data Action = DefinedAction PreDefined | FunctionAction Definition deriving (Show, Generic)
data Pre-Defined = Click | Hover deriving (Show, Generic)

-- still have to define parsers


instance ToJSON Card
instance ToJSON Rule
instance ToJSON Action
instance ToJSON Game

-- Parsers
--gameParser :: Parser String
--gameParser = do
--  string "game" >> spaces
--  char '"'
--  gName <- manyTill anyChar (char '"')
--  spaces
--  return gName
--
--playersParser :: Parser Int
--playersParser = do
--  string "players" >> spaces
--  pCount <- read <$> many1 digit
--  spaces
--  return pCount
--
--cardParser :: Parser Card
--cardParser = do
--  string "card" >> spaces
--  char '"'
--  cName <- manyTill anyChar (char '"')
--  spaces
--  string "value" >> spaces
--  cValue <- read <$> many1 digit
--  spaces
--  string "color" >> spaces
--  cColor <- choice [char '"' >> manyTill anyChar (char '"'), many1 letter]
--  spaces
--  return $ Card cName cValue cColor
--
--
--definitionParser :: Parser Definition
--definitionParser = do
--  string "define" >> spaces
--  char '"'
--  typ <- manyTill anyChar (char '"')
--  spaces
--  string "winner" >> spaces
--  char '"'
--  description <- manyTill anyChar (char '"')
--  spaces
--  return $ Definition typ description
--
--actionParser :: Parser Action
--actionParser = do
--  string "action" >> spaces
--  char '"'
--  aName <- manyTill anyChar (char '"')
--  spaces
--  return $ Action aName
--
--gameFileParser :: Parser Game
--gameFileParser = do
--  gName <- gameParser
--  pCount <- playersParser
--  cardList <- many cardParser
--  definitionList <- many definitionParser
--  actionList <- many actionParser
--  return $ Game gName pCount cardList definitionList actionList

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
--parseGameFile :: FilePath -> IO ()
--parseGameFile file = do
--  content <- readFile file
--  case parse gameFileParser "" content of
--    Left err -> do
--      putStrLn "Error while parsing the file:"
--      print err
--    Right game -> do
--      B.writeFile "C:/Users/user/Desktop/cards/game.json" (encode game)
--      -- mapM_ generateCardImage (cards game)
--      putStrLn "Game parsed and saved to game.json"

sizeParser :: Parser Size
sizeParser = do
  width <- read <$> many1 digit
  spaces
  char 'x'
  spaces
  height <- read <$> many1 digit
  spaces
  return Size width height

optionParser :: Parser Option
optionParser = do
  string "game-name" >> spaces
  char '"'
  gameName <- manyTill anyChar (char '"')
  spaces
  return GameName gameName
  <|>
  string "players" >> spaces
  playerCount <- read <$> many1 digit
  return Players playerCount
  <|>
  string "window-size" >> spaces
  char '='
  spaces
  size <- sizeParser
  spaces
  return WindowSize size
  <|>
  string "default-card-size" >> spaces
  char '='
  spaces
  size <- sizeParser
  spaces
  return CardSize size
  <|>
  string "default-image-size" >> spaces
  char '='
  spaces
  size <- sizeParser
  spaces
  return ImageSize size

settingParser :: Parser Setting
settingParser = do
  string "set" >> spaces
  option <- optionParser
  spaces
  return Setting option

listParser :: Parser [String]
listParser = do
  char '[' >> spaces
  ?????

typeParser :: Parser Type
typeParser = do
  string "type card" >> spaces
  char '"'
  typeName <- manyTill anyChar (char '"')
  spaces
  attributes <- listParser
  spaces
  return Type typeName attributes

creationParser :: Parser Creation
creationParser = do
  string "card" >> spaces
  char '"'
  cardName <- manyTill anyChar (char '"')
  spaces
  ?????
  return Creation cardName ...

definitionParser :: Parser Definition
definitionParser = do
  string "define" >> spaces
  char '"'
  functionName <- manyTill anyChar (char '"')
  spaces
  string "takes"
  spaces
  ??????
  return Definition functionName ...

-- probably, no very likely, not necessary, should be changed
actionParser :: Parser Action
actionParser = do
  char '"' >> spaces
  ???
  spaces
  return FunctionAction ???
  <|>
  preDefined <- preDefinedParser

triggerParser :: Parser Trigger
triggerParser = do
  string "trigger" >> spaces
  function <- ???
  spaces
  string "on"
  spaces
  action <- actionParser
  spaces
  ????

lineParser :: Parser Line
lineParser = do
  line <- settingParser
  return SettingLine line
  <|>
  line <- typeParser
  return TypeLine line
  <|>
  line <- creationParser
  return CreationLine line
  <|>
  line <- definitionParser
  return DefinitionLine line
  <|>
  line <- triggerParser
  return TriggerLine line

setupParser :: Parser Setup
setupParser = do

loopParser :: Parser Loop
loopParser = do

programParser :: Parser Program
programParser = do

