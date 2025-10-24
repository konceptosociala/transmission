{-# LANGUAGE PatternSynonyms #-}
module Main where

import Raylib.Util (withWindow, whileWindowOpen_, mode3D, drawing, managed)
import Raylib.Core (clearBackground, disableCursor, toggleFullscreen, getRenderWidth, getRenderHeight)
import Raylib.Types (pattern Vector3, Color (Color))
import Raylib.Core.Models (drawModelEx, loadModelFromMesh)
import Raylib.Util.Colors (white, black)
import Raylib.Core.Text (drawFPS)
import Control.Monad (when)
import GameState (Scene(ScnMainMenu, ScnGame, ScnOptions), State (showFps, currentScene, camera), initState, updateState)
import Utils (loadTexturedModel, drawButton, todo')
import MainMenu (SceneMainMenu(SceneMainMenu), MainMenuItem (..))
import Level (cube)

title :: String
title = "T.R.A.N.S.M.I.S.S.I.O.N"

skyColor :: Color
skyColor = Color 171 214 255 255

main :: IO ()
main = do
   withWindow 800 600 title 60 $ \w -> do
      disableCursor
      toggleFullscreen

      logo <- loadTexturedModel w "assets/logo.obj" "assets/logo.png"
      cube <- managed w $ loadModelFromMesh =<< cube

      flip whileWindowOpen_ initState $
         \s -> do
            screenSize <- (,) <$> getRenderWidth <*> getRenderHeight
            drawing $ do
               case currentScene s of
                  ScnMainMenu (SceneMainMenu item rot) -> do
                     clearBackground black
                     mode3D (camera s) $ do
                        -- drawModelEx logo (Vector3 0 2 0) (Vector3 0 1 0) rot 0.5 white
                        drawModelEx cube (Vector3 0 1 0) (Vector3 1 1 1) rot 0.5 white

                     drawButton "New game" screenSize (-60) MmiNewGame item
                     drawButton "Options" screenSize 0 MmiOptions item
                     drawButton "Exit" screenSize 60 MmiExit item

                  ScnGame _ -> do
                     clearBackground skyColor

                  ScnOptions _ -> todo' "draw options"

               when (showFps s) $ drawFPS 0 0

            updateState s