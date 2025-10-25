{-# LANGUAGE RecordWildCards #-}
module MainMenu where

import Raylib.Core (isWindowFullscreen, toggleFullscreen, isKeyPressed, getFrameTime)
import Control.Monad (when)
import Raylib.Types (KeyboardKey(..))
import Data.Function ((&))

data SceneMainMenu = SceneMainMenu
   { mmSelectedItem :: MainMenuItem
   , mmItemClicked :: Bool
   , mmLogoRotation :: Float
   }

data SceneSingleplayer = SceneSingleplayer

data SceneConnect = SceneConnect

data MainMenuItem
   = MmiSingleplayer
   | MmiConnect
   | MmiLevelEditor
   | MmiOptions
   | MmiExit
   deriving Eq

updateMainMenu :: SceneMainMenu -> IO SceneMainMenu
updateMainMenu (SceneMainMenu item _ rot) = do
   down  <- isKeyPressed KeyDown
   up    <- isKeyPressed KeyUp
   enter <- isKeyPressed KeyEnter
   let mmSelectedItem = item & if up
         then prevMenuItem
         else if down
            then nextMenuItem
            else id

   dt <- getFrameTime

   let mmLogoRotation = rot + (32.0 * dt)
   let mmItemClicked = enter

   return SceneMainMenu {..}

setFullScreen :: Bool -> IO ()
setFullScreen should = do
   isFullscreen <- isWindowFullscreen
   when 
      ( (isFullscreen && not should) 
            || (not isFullscreen && should)
      ) 
      toggleFullscreen

nextMenuItem :: MainMenuItem -> MainMenuItem
nextMenuItem MmiSingleplayer = MmiConnect
nextMenuItem MmiConnect      = MmiLevelEditor
nextMenuItem MmiLevelEditor  = MmiOptions
nextMenuItem MmiOptions      = MmiExit
nextMenuItem MmiExit         = MmiSingleplayer

prevMenuItem :: MainMenuItem -> MainMenuItem
prevMenuItem MmiSingleplayer = MmiExit
prevMenuItem MmiConnect      = MmiSingleplayer
prevMenuItem MmiLevelEditor  = MmiConnect
prevMenuItem MmiOptions      = MmiLevelEditor
prevMenuItem MmiExit         = MmiOptions