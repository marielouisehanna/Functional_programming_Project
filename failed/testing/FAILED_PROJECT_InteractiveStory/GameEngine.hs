module GameEngine (
  playScene,
  findScene
) where

import GameData

-- Play a single scene
playScene :: Scene -> [Scene] -> IO ()
playScene (Scene _ text choices) allScenes = do
  putStrLn text
  putStrLn "Choices:"
  mapM_ (\(i, Choice t _) -> putStrLn $ show i ++ ". " ++ t) (zip [1..] choices)
  putStr "Enter your choice: "
  input <- getLine
  case reads input :: [(Int, String)] of
    [(n, "")] | n > 0 && n <= length choices -> do
      let nextId = nextScene (choices !! (n - 1))
      let next = findScene nextId allScenes
      maybe (putStrLn "Invalid scene!") (`playScene` allScenes) next
    _ -> do
      putStrLn "Invalid choice. Try again!"
      playScene (Scene input text choices) allScenes

-- Find a scene by its ID
findScene :: String -> [Scene] -> Maybe Scene
findScene sid = foldr (\s acc -> if sceneId s == sid then Just s else acc) Nothing
