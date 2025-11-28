module Render.Game where

import Raylib.Core
import Scene.Game (SceneGame)
import Constants

renderGame :: SceneGame -> IO ()
renderGame _game = do
   clearBackground skyColor