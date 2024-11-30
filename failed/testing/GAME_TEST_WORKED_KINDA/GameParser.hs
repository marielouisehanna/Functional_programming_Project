module Main where

import Text.Parsec
import Text.Parsec.String (Parser)

-- Data Types
data Creature = Creature
  { name    :: String
  , hp      :: Int
  , attack  :: Maybe (String, Int)
  } deriving (Show)

data Action = Attack String String | Heal String | Buff String deriving (Show)

data Turn = Turn String [Action] deriving (Show)

data Game = Game
  { creatures :: [Creature]
  , turns     :: [Turn]
  } deriving (Show)

-- Utility: Skip optional whitespace (spaces, tabs, and newlines)
whitespace :: Parser ()
whitespace = skipMany (space <|> newline <|> tab)

-- Parse a Creature
parseCreature :: Parser Creature
parseCreature = do
  whitespace
  _ <- string "CREATURE"
  whitespace
  creatureName <- many1 letter
  whitespace
  _ <- char '{'
  whitespace
  hpValue <- optionMaybe parseHP
  attackValue <- optionMaybe parseAttack
  whitespace
  _ <- char '}'
  whitespace
  return $ Creature creatureName (maybe 100 id hpValue) attackValue

parseHP :: Parser Int
parseHP = do
  _ <- string "HP:"
  whitespace
  hpValue <- many1 digit
  whitespace
  return $ read hpValue

parseAttack :: Parser (String, Int)
parseAttack = do
  _ <- string "ATTACK:"
  whitespace
  attackName <- many1 letter
  whitespace
  _ <- char ','
  whitespace
  _ <- string "DAMAGE:"
  whitespace
  damageValue <- many1 digit
  whitespace
  return (attackName, read damageValue)

-- Parse Actions
parseAction :: Parser Action
parseAction = try parseAttackAction <|> try parseHealAction <|> parseBuffAction

parseAttackAction :: Parser Action
parseAttackAction = do
  _ <- string "ATTACK"
  whitespace
  attacker <- many1 letter
  whitespace
  _ <- string "->"
  whitespace
  target <- many1 letter
  whitespace
  return $ Attack attacker target

parseHealAction :: Parser Action
parseHealAction = do
  _ <- string "HEAL"
  whitespace
  target <- many1 letter
  whitespace
  return $ Heal target

parseBuffAction :: Parser Action
parseBuffAction = do
  _ <- string "BUFF"
  whitespace
  target <- many1 letter
  whitespace
  return $ Buff target

-- Parse Turns
parseTurn :: Parser Turn
parseTurn = do
  whitespace
  _ <- string "TURN"
  whitespace
  playerName <- many1 letter
  whitespace
  _ <- char '{'
  whitespace
  actions <- many parseAction
  whitespace
  _ <- char '}'
  whitespace
  return $ Turn playerName actions

-- Parse the Entire Game
parseGame :: Parser Game
parseGame = do
  creatures <- many parseCreature
  whitespace
  turns <- many parseTurn
  whitespace
  return $ Game creatures turns

-- Write Output Files
writeCreatures :: [Creature] -> IO ()
writeCreatures creatures = writeFile "creatures.txt" $ unlines $ map showCreature creatures

writeTurns :: [Turn] -> IO ()
writeTurns turns = writeFile "turns.txt" $ unlines $ map showTurn turns

showCreature :: Creature -> String
showCreature (Creature n h (Just (a, d))) = n ++ "," ++ show h ++ "," ++ a ++ "," ++ show d
showCreature (Creature n h Nothing) = n ++ "," ++ show h

showTurn :: Turn -> String
showTurn (Turn player actions) =
  player ++ ": " ++ unwords (map showAction actions)

showAction :: Action -> String
showAction (Attack a t) = "ATTACK " ++ a ++ " -> " ++ t
showAction (Heal t) = "HEAL " ++ t
showAction (Buff t) = "BUFF " ++ t

-- Main Function
main :: IO ()
main = do
  input <- readFile "simple_game.txt"
  putStrLn "Input file content:"
  putStrLn input
  case parse parseGame "" input of
    Left err -> do
      putStrLn $ "Parse error: " ++ show err
    Right (Game creatures turns) -> do
      writeCreatures creatures
      writeTurns turns
      putStrLn "Parsing completed. Output written to creatures.txt and turns.txt."
