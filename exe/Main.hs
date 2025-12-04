module Main where

import Control.Monad (when)

import Raylib.Util (withWindow, drawing)
import Raylib.Core
import Raylib.Types
import Raylib.Core.Text (drawFPS)
import Raylib.Core.Audio

import Sounds
import GameState
import Utils
import Constants

import Scene.Options

import Render.MainMenu
import Render.Options
import Render.LevelEditorSelect
import Render.LevelEditor
import Render.Game
import Render.NewLevel
import Render.Connect
import Render.Singleplayer

main :: IO ()
main = do
   withWindow windowWidth windowHeight title targetFps $ \win -> do
      initAudioDevice
      disableCursor
      setExitKey KeyNull

      crosshair   <- loadCrosshair win
      logo        <- loadTexturedModel win "assets/logo.obj" "assets/logo.png"
      options     <- loadOrCreateOptions
      mainMat     <- loadMainMaterial win

      sounds <- loadSounds win
      updateSounds sounds options
      setFullScreen (isFullscreen options)

      whileWindowOpen (initState sounds options win) $
         \s -> do
            screenSize     <- (,) <$> getRenderWidth <*> getRenderHeight
            menuBgPlaying  <- isMusicStreamPlaying $ mscMenuBg sounds

            when menuBgPlaying
               $ updateMusicStream $ mscMenuBg sounds

            drawing $ do
               case currentScene s of
                  ScnMainMenu mainMenu -> 
                     renderMainMenu mainMenu screenSize logo (camera s)

                  ScnGame game -> do
                     renderGame game mainMat

                  ScnOptions optionsScn -> 
                     renderOptions optionsScn screenSize

                  ScnLevelEditorSelect levelEditorSelect -> 
                     renderLevelEditorSelect levelEditorSelect screenSize

                  ScnLevelEditor levelEditor ->
                     renderLevelEditor levelEditor screenSize crosshair mainMat (camera s)
   
                  ScnNewLevel newLevel ->
                     renderNewLevel newLevel screenSize

                  ScnSingleplayer singleplayer ->
                     renderSingleplayer singleplayer screenSize

                  ScnConnect connect ->
                     renderConnect connect

                  ScnExit -> do
                     closeAudioDevice (Just win)
                     return ()

               when (showFps s) $ drawFPS 0 0

            updateState s

whileWindowOpen :: State -> (State -> IO State) -> IO ()
whileWindowOpen state f = do
   newState    <- f state
   shouldClose <- windowShouldClose

   if shouldClose || isExitState newState
      then return ()
      else whileWindowOpen newState f