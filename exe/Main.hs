{-# LANGUAGE PatternSynonyms #-}
module Main where

import Raylib.Util (withWindow, mode3D, drawing)
import Raylib.Core (clearBackground, disableCursor, toggleFullscreen, getRenderWidth, getRenderHeight, setExitKey, windowShouldClose)
import Raylib.Types (pattern Vector3, Color (Color), KeyboardKey (KeyNull))
import Raylib.Core.Models (drawModelEx, drawGrid, drawCube)
import Raylib.Core.Text (drawFPS)
import Control.Monad (when)
import GameState (Scene(..), State (showFps, currentScene, camera), initState, updateState, isExitState)
import Utils (loadTexturedModel, drawButton, todo')
import MainMenu (SceneMainMenu(SceneMainMenu), MainMenuItem (..))
import Raylib.Util.Colors
import Level (Level (..), Dims (Dims), Block (BFinish, BPunch, BHeight), Direction (DirFront))
import Level.Manipulate (newLevel, setBlock, freezeLevel, serializeLevel, deserializeLevel)
import qualified Data.ByteString as BS
import Text.Megaparsec (errorBundlePretty)

title :: String
title = "T.R.A.N.S.M.I.S.S.I.O.N"

skyColor :: Color
skyColor = Color 171 214 255 255

main :: IO ()
main = do
   let filename = "test.lvl"

   let dims = Dims 2 2 2
   ml <- newLevel dims
   setBlock ml (0, 0, 0) BFinish
   setBlock ml (1, 0, 0) BPunch
   setBlock ml (0, 1, 0) $ BHeight DirFront 3
   lvl <- freezeLevel ml
   print lvl
   BS.writeFile filename $ serializeLevel lvl

   loaded <- BS.readFile filename
   let lvlDe = deserializeLevel loaded
   case lvlDe of
      Left err -> putStrLn $ errorBundlePretty err
      Right lvl2 -> print lvl2

   return ()



main_ :: IO ()
main_ = do
   withWindow 800 600 title 60 $ \w -> do
      disableCursor
      toggleFullscreen
      setExitKey KeyNull

      logo <- loadTexturedModel w "assets/logo.obj" "assets/logo.png"
      loadedLevels <- loadLevels

      whileWindowOpen (initState loadedLevels) $
         \s -> do
            screenSize <- (,) <$> getRenderWidth <*> getRenderHeight
            drawing $ do
               case currentScene s of
                  ScnMainMenu (SceneMainMenu item _ rot) -> do
                     clearBackground black
                     mode3D (camera s) $
                        drawModelEx logo (Vector3 0 2 0) (Vector3 0 1 0) rot 0.5 white

                     drawButton "Singleplayer"  screenSize (-120) MmiSingleplayer item
                     drawButton "Connect..."    screenSize (-60)  MmiConnect      item
                     drawButton "Level Editor"  screenSize 0      MmiLevelEditor  item
                     drawButton "Options"       screenSize 60     MmiOptions      item
                     drawButton "Exit"          screenSize 120    MmiExit         item

                  -- ScnSingleplayer -> do
                  --    if 

                  ScnGame _ -> do
                     clearBackground skyColor

                  ScnLevelEditor _ -> do
                     clearBackground black
                     mode3D (camera s) $ do
                        drawGrid 10 1.0
                        drawCube (Vector3 0 0 0) 2 2 2 red

                  ScnExit -> return ()
                  _ -> todo' "draw another scenes"

               when (showFps s) $ drawFPS 0 0

            updateState s

loadLevels :: IO [Level]
loadLevels = pure []

whileWindowOpen :: State -> (State -> IO State) -> IO ()
whileWindowOpen state f = do
   newState    <- f state
   shouldClose <- windowShouldClose

   if shouldClose || isExitState newState
      then return ()
      else whileWindowOpen newState f