{-# LANGUAGE RecordWildCards #-}
module MainMenu where

import Raylib.Core (isWindowFullscreen, toggleFullscreen, isKeyPressed, getFrameTime)
import Control.Monad (when)
import Raylib.Types (KeyboardKey(..))
import Data.Function ((&))

data SceneMainMenu = SceneMainMenu
   { mmSelectedItem :: MainMenuItem
   , mmLogoRotation :: Float
   }

data MainMenuItem
   = MmiNewGame
   | MmiOptions
   | MmiExit
   deriving Eq

updateMainMenu :: SceneMainMenu -> IO SceneMainMenu
updateMainMenu (SceneMainMenu item rot) = do
   down  <- isKeyPressed KeyDown
   up    <- isKeyPressed KeyUp
   let mmSelectedItem = item & if up
         then prevMenuItem
         else if down
            then nextMenuItem
            else id

   dt <- getFrameTime

   let mmLogoRotation = rot + (32.0 * dt)

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
nextMenuItem MmiNewGame = MmiOptions
nextMenuItem MmiOptions = MmiExit
nextMenuItem MmiExit    = MmiNewGame

prevMenuItem :: MainMenuItem -> MainMenuItem
prevMenuItem MmiNewGame = MmiExit
prevMenuItem MmiOptions = MmiNewGame
prevMenuItem MmiExit    = MmiOptions