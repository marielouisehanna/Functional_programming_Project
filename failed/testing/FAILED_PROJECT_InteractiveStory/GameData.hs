module GameData where

-- A single scene in the game
data Scene = Scene
  { sceneId   :: String      -- Unique ID for the scene
  , sceneText :: String      -- Text description of the scene
  , choices   :: [Choice]    -- Choices available in the scene
  } deriving (Show)

-- A choice within a scene
data Choice = Choice
  { choiceText :: String     -- Text shown for this choice
  , nextScene  :: String     -- ID of the next scene
  } deriving (Show)
