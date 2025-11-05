{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
module MainMenu where

import Raylib.Core (isWindowFullscreen, toggleFullscreen, isKeyPressed, getFrameTime)
import Control.Monad (when, unless)
import Raylib.Types (pattern Vector3, CameraProjection (CameraPerspective), KeyboardKey(..), Camera3D (..), Rectangle (Rectangle))
import Data.Function
import Raylib.Core.Shapes (drawRectangleLinesEx)
import Raylib.Util.Colors (red)
import Utils (drawButton)
import Raylib.Core.Text (drawText)
import Sounds (Sounds (sndHover, sndClick, mscMenuBg))
import Raylib.Core.Audio (playSound, isMusicStreamPlaying, playMusicStream)

newtype MsgBox = MsgBox String

mkMainMenu :: SceneMainMenu
mkMainMenu = SceneMainMenu
   { mmSelectedItem = MmiSingleplayer
   , mmItemClicked = False
   , mmLogoRotation = 0
   , mmMsgBox = Nothing
   }

withMsgBox :: MsgBox -> SceneMainMenu -> SceneMainMenu
withMsgBox msgbox sc = sc
   { mmMsgBox = Just msgbox
   }

mainMenuCam :: Camera3D
mainMenuCam = Camera3D
   { camera3D'position = Vector3 3 1 0
   , camera3D'target = Vector3 0 1 0
   , camera3D'up = Vector3 0 1 0
   , camera3D'fovy = 70
   , camera3D'projection = CameraPerspective
   };

data SceneMainMenu = SceneMainMenu
   { mmSelectedItem :: MainMenuItem
   , mmItemClicked :: Bool
   , mmLogoRotation :: Float
   , mmMsgBox :: Maybe MsgBox
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

checkMsgBox :: IO Bool
checkMsgBox = isKeyPressed KeyEnter

drawMsgBox :: (Int, Int) -> MsgBox -> IO ()
drawMsgBox size@(w, h) (MsgBox label) = do
   let x = fromIntegral (w - 600) / 2
   let y = fromIntegral (h - 420) / 2

   drawRectangleLinesEx (Rectangle x y 600 420) 2 red
   drawText "Error" (floor x + 20) (floor y + 20) 40 red
   drawText label (floor x + 20) (floor y + 80) 20 red
   drawButton "Okay" size 120 () ()

updateMainMenu :: SceneMainMenu -> Sounds -> IO SceneMainMenu
updateMainMenu (SceneMainMenu item _ rot msgbox) sound' = do
   isMusic <- isMusicStreamPlaying $ mscMenuBg sound'

   unless isMusic
      $ playMusicStream $ mscMenuBg sound'

   case msgbox of
      Just msg -> do
         ok <- checkMsgBox

         return $ SceneMainMenu
            { mmSelectedItem = item
            , mmItemClicked = False
            , mmLogoRotation = rot
            , mmMsgBox = if ok then Nothing else Just msg
            }

      Nothing -> do
         down  <- isKeyPressed KeyDown
         up    <- isKeyPressed KeyUp
         enter <- isKeyPressed KeyEnter

         when (down || up)
            $ playSound $ sndHover sound'

         when enter
            $ playSound $ sndClick sound'

         let mmSelectedItem = item & if up
               then prevMenuItem
               else if down
                  then nextMenuItem
                  else id

         dt <- getFrameTime

         let mmLogoRotation = rot + (32.0 * dt)
         let mmItemClicked = enter
         let mmMsgBox = Nothing

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