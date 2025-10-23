{-# LANGUAGE PatternSynonyms #-}
module GameState where

import Raylib.Core (isKeyPressed)
import Raylib.Core.Camera (updateCamera)
import Raylib.Types (CameraMode(CameraModeFirstPerson), Camera3D(..), pattern Vector3, CameraProjection (CameraPerspective), KeyboardKey (KeyA, KeyF3))
import Data.Function
import MainMenu
import Options
import Game
import Utils (todo')

data State = State
   { camera :: Camera3D
   , currentScene :: Scene
   , showFps :: Bool
   }

data Scene
   = ScnMainMenu SceneMainMenu
   | ScnOptions SceneOptions
   | ScnGame SceneGame

initState :: State
initState = State
   { showFps = False
   , camera = Camera3D
      { camera3D'position = Vector3 3 1 0
      , camera3D'target = Vector3 0 1 0
      , camera3D'up = Vector3 0 1 0
      , camera3D'fovy = 70
      , camera3D'projection = CameraPerspective
      }
   , currentScene = ScnMainMenu $ SceneMainMenu
      { mmSelectedItem = MmiNewGame
      , mmLogoRotation = 0
      }
   }

updateState :: State -> IO State
updateState state = do
   f3 <- isKeyPressed KeyF3

   let showFps_ = if f3 then not previous else previous
         where previous = showFps state

   switch <- isKeyPressed KeyA
   currentScene_ <- currentScene state
      & switchScene switch
      & updateScene

   camera_ <-  case currentScene state of
      ScnGame _ -> updateCamera (camera state) CameraModeFirstPerson
      _         -> pure $ camera state

   return state
      { camera       = camera_
      , currentScene = currentScene_
      , showFps      = showFps_
      }

switchScene :: Bool -> Scene -> Scene
switchScene switch current =
   if switch
      then
         case current of
            ScnMainMenu _ -> ScnGame SceneGame
            ScnGame _ -> ScnMainMenu $ SceneMainMenu MmiNewGame 0
            _ -> todo' "switch options"
      else
         current

updateScene :: Scene -> IO Scene
updateScene (ScnGame _) = pure $ ScnGame SceneGame
updateScene (ScnMainMenu mainMenu) = ScnMainMenu <$> updateMainMenu mainMenu
updateScene _ = todo' "update options"