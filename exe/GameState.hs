{-# LANGUAGE PatternSynonyms #-}
module GameState where

import Raylib.Core (isKeyPressed)
import Raylib.Types (Camera3D(..), pattern Vector3, CameraProjection (CameraPerspective), KeyboardKey (KeyF3))
import MainMenu
import Options
import Game
import Utils (todo')
import LevelEditor
import Data.Function
import Level (Level)

data State = State
   { camera :: Camera3D
   , loadedLevels :: [Level]
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

mainMenuCam :: Camera3D
mainMenuCam = Camera3D
   { camera3D'position = Vector3 3 1 0
   , camera3D'target = Vector3 0 1 0
   , camera3D'up = Vector3 0 1 0
   , camera3D'fovy = 70
   , camera3D'projection = CameraPerspective
   };

getCamera :: Scene -> Camera3D
getCamera sc = case sc of
   ScnMainMenu _     -> mainMenuCam
   ScnOptions _      -> mainMenuCam
   ScnConnect _      -> mainMenuCam
   ScnExit           -> mainMenuCam
   ScnSingleplayer _ -> mainMenuCam
   ScnLevelEditor (SceneLevelEditor cam) -> cam
   ScnGame _ -> todo' "game camera"


isExitState :: State -> Bool
isExitState state = 
   case currentScene state of
      ScnExit -> True
      _       -> False

initState :: [Level] -> State
initState loadedLevels = State
   { showFps = False
   , loadedLevels = loadedLevels
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
                  MmiLevelEditor  -> ScnLevelEditor mkSceneLevelEditor
                  MmiOptions      -> ScnOptions (SceneOptions OptMusicVolume)
                  MmiExit         -> ScnExit
               else initial
         other -> other

   let camera_ = getCamera currentScene_

   return state
      { camera       = camera_
      , currentScene = currentScene_
      , showFps      = showFps_
      }

updateScene :: Scene -> IO Scene
updateScene (ScnGame _)             = ScnGame SceneGame  & pure
updateScene (ScnMainMenu mainMenu)  = ScnMainMenu       <$> updateMainMenu mainMenu
updateScene (ScnLevelEditor editor) = ScnLevelEditor    <$> updateLevelEditor editor
updateScene ScnExit                 = ScnExit            & pure 
updateScene _ = todo' "update other scenes"