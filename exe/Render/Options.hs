module Render.Options where

import Raylib.Core
import Raylib.Util.Colors

import Utils
import Scene.Options (OptionsItem (..), Options (..), SceneOptions (..))

renderOptions :: SceneOptions -> (Int, Int) -> IO ()
renderOptions (SceneOptions sel opts _) screenSize = do
   clearBackground black
   drawTextCentered "Options" screenSize (-240) 48 white

   drawButton ("Music Volume: " ++ show (musicVolume opts)) 
      screenSize (-180) OptMusicVolume sel
   drawButton ("Sound Volume: " ++ show (soundVolume opts)) 
      screenSize (-120) OptSoundVolume sel
   drawButton ("Fullscreen: " ++ if isFullscreen opts then "On" else "Off") 
      screenSize (-60) OptFullscreen sel

   drawButton "Save" 
      screenSize 40 OptSave sel
   drawButton "Cancel" 
      screenSize 100 OptCancel sel