{-# LANGUAGE PatternSynonyms #-}
module GameState where

import Raylib.Core (isKeyPressed)
import Raylib.Core.Camera (updateCamera)
import Raylib.Types (CameraMode(CameraModeFirstPerson), Camera3D(..), pattern Vector3, CameraProjection (CameraPerspective), KeyboardKey (KeyF3))
import MainMenu
import Options
import Game
import Utils (todo')
import LevelEditor

data State = State
   { camera :: Camera3D
   , currentScene :: Scene
   , showFps :: Bool
   }

data Scene
   = ScnMainMenu SceneMainMenu
   | ScnOptions SceneOptions
   | ScnSingleplayer SceneSingleplayer
   | ScnConnect SceneConnect
   | ScnGame SceneGame
   | ScnLevelEditor SceneLevelEditor
   | ScnExit

isExitState :: State -> Bool
isExitState state = 
   case currentScene state of
      ScnExit -> True
      _       -> False

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
      { mmSelectedItem = MmiSingleplayer
      , mmItemClicked = False
      , mmLogoRotation = 0
      }
   }

updateState :: State -> IO State
updateState state = do
   f3 <- isKeyPressed KeyF3

   let showFps_ = if f3 then not previous else previous
         where previous = showFps state

   updatedScene <- updateScene $ currentScene state

   let currentScene_ = case updatedScene of
         initial@(ScnMainMenu (SceneMainMenu item selected _)) -> 
            if selected 
               then case item of
                  MmiSingleplayer -> ScnSingleplayer SceneSingleplayer
                  MmiConnect      -> ScnConnect SceneConnect
                  MmiLevelEditor  -> ScnLevelEditor SceneLevelEditor
                  MmiOptions      -> ScnOptions (SceneOptions OptMusicVolume)
                  MmiExit         -> ScnExit
               else initial
               

         other -> other

   camera_ <- case currentScene state of
      ScnGame _ -> updateCamera (camera state) CameraModeFirstPerson
      _         -> pure $ camera state

   return state
      { camera       = camera_
      , currentScene = currentScene_
      , showFps      = showFps_
      }

updateScene :: Scene -> IO Scene
updateScene (ScnGame _) = pure $ ScnGame SceneGame
updateScene (ScnMainMenu mainMenu) = ScnMainMenu <$> updateMainMenu mainMenu
updateScene ScnExit = pure ScnExit
updateScene _ = todo' "update other scenes"