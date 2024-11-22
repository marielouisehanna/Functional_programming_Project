module GameParser (
  sceneParser,
  choiceParser,
  gameParser
) where

import Parsing
import Token
import GameData

-- Parse a single choice
choiceParser :: Parser Choice
choiceParser = do
  _ <- symbol "Choice"
  text <- many (satisfy (/= ':')) -- Parse until a colon
  _ <- symbol ":"
  next <- identifier -- Parse the next scene ID
  return $ Choice text next

-- Parse a single scene
sceneParser :: Parser Scene
sceneParser = do
  _ <- symbol "Scene"
  sid <- identifier -- Parse the scene ID
  _ <- symbol "{"
  desc <- many (satisfy (/= '}')) -- Parse the scene description
  _ <- symbol "}"
  choices <- many choiceParser -- Parse the choices
  return $ Scene sid desc choices

-- Parse the entire game script
gameParser :: Parser [Scene]
gameParser = many sceneParser
