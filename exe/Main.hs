{-# LANGUAGE PatternSynonyms #-}

module Main where

import Paths_transmission (getDataFileName)
import Raylib.Util (withWindow, whileWindowOpen_, managed, mode3D, drawing)
import Raylib.Core (clearBackground, isKeyPressed)
import Raylib.Core.Camera (updateCamera)
import Raylib.Types (CameraMode(CameraModeOrbital, CameraModeFirstPerson), Camera3D (Camera3D), pattern Vector3, CameraProjection (CameraPerspective), Color (Color), KeyboardKey (KeyA, KeyF3))
import Raylib.Core.Models (drawModel, loadModel)
import Raylib.Util.Colors (white, black)
import Raylib.Core.Text (drawFPS)
import Control.Monad (when)

skyColor :: Color
skyColor = Color 171 214 255 255

data MainMenuItem
   = MmiNewGame
   | MmiOptions
   | MmiExit

data Scene
   = ScnMainMenu
      { mmSelectedItem :: MainMenuItem
      }
   | ScnGame
      {
      }

data State = State
   { camera :: Camera3D
   , currentScene :: Scene
   , showFps :: Bool
   }

main :: IO ()
main = do
   withWindow
      800 600
      "T.R.A.N.S.M.I.S.S.I.O.N"
      60
      $ \window -> do
         logoModel <- managed window $ loadModel =<< getDataFileName "assets/logo.obj"

         flip whileWindowOpen_ initState $
            \s -> do
               drawing $ do
                  case currentScene s of
                     ScnMainMenu _ -> do
                        clearBackground black
                        mode3D (camera s) $ do
                           drawModel logoModel (Vector3 0 2 0) 1 white

                     ScnGame -> do
                        clearBackground skyColor

                  when (showFps s) $ drawFPS 0 0

               updateState s

initState :: State
initState = State
   { showFps = False
   , camera = Camera3D (Vector3 3 2 3) (Vector3 0 0 0) (Vector3 0 1 0) 70 CameraPerspective
   , currentScene = ScnMainMenu
      { mmSelectedItem = MmiNewGame
      }
   }

updateState :: State -> IO State
updateState state = do
   f3 <- isKeyPressed KeyF3

   let showFps_ = if f3 then not previous else previous
         where previous = showFps state

   switch <- isKeyPressed KeyA
   let currentScene_ = if switch
         then
            case currentScene state of
               ScnMainMenu _ -> ScnGame
               ScnGame -> ScnMainMenu MmiNewGame
         else
            currentScene state

   camera_ <- updateCamera (camera state) $ case currentScene state of
      ScnMainMenu _ -> CameraModeOrbital
      ScnGame       -> CameraModeFirstPerson

   return state 
      { camera       = camera_
      , currentScene = currentScene_
      , showFps      = showFps_
      }
