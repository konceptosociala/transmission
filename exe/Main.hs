{-# LANGUAGE PatternSynonyms #-}
module Main where

import Raylib.Util (withWindow, mode3D, drawing)
import Raylib.Core (clearBackground, disableCursor, toggleFullscreen, getRenderWidth, getRenderHeight, setExitKey, windowShouldClose)
import Raylib.Types (pattern Vector3, Color (Color), KeyboardKey (KeyNull))
import Raylib.Core.Models (drawModelEx, drawGrid, drawCube)
import Raylib.Core.Text (drawFPS)
import Control.Monad (when, forM_)
import GameState (Scene(..), State (showFps, currentScene, camera), initState, updateState, isExitState)
import Utils (loadTexturedModel, drawButton, todo', drawTextCentered, drawTextInput, showOrEmpty)
import MainMenu (SceneMainMenu(SceneMainMenu), MainMenuItem (..), drawMsgBox)
import Raylib.Util.Colors
import System.Directory (getDirectoryContents, createDirectoryIfMissing)
import System.FilePath (takeExtension)
import LevelEditor (LevelDescr (..), SceneLevelEditorSelect (SceneLevelEditorSelect), SceneNewLevel (SceneNewLevel), SNLItem (..))
import Level (Dims(Dims))
import Sounds (loadSounds, Sounds (mscMenuBg))
import Raylib.Core.Audio (initAudioDevice, closeAudioDevice, updateMusicStream, isMusicStreamPlaying)
import Options (loadOrCreateOptions)

title :: String
title = "T.R.A.N.S.M.I.S.S.I.O.N"

skyColor :: Color
skyColor = Color 171 214 255 255

main :: IO ()
main = do
   withWindow 800 600 title 60 $ \win -> do
      initAudioDevice
      disableCursor
      toggleFullscreen
      setExitKey KeyNull

      logo         <- loadTexturedModel win "assets/logo.obj" "assets/logo.png"
      sounds       <- loadSounds win
      loadedLevels <- loadLevels
      options      <- loadOrCreateOptions

      whileWindowOpen (initState loadedLevels sounds options) $
         \s -> do
            menuBgPlaying <- isMusicStreamPlaying $ mscMenuBg sounds
            when menuBgPlaying
               $ updateMusicStream $ mscMenuBg sounds

            screenSize <- (,) <$> getRenderWidth <*> getRenderHeight
            drawing $ do
               case currentScene s of
                  ScnMainMenu (SceneMainMenu item _ rot msgbox) -> do
                     clearBackground black
                     case msgbox of
                        Just mb -> drawMsgBox screenSize mb
                        Nothing -> do
                           mode3D (camera s) $
                              drawModelEx logo (Vector3 0 2 0) (Vector3 0 1 0) rot 0.5 white

                           drawButton "Singleplayer"  screenSize (-120) MmiSingleplayer item
                           drawButton "Connect..."    screenSize (-60)  MmiConnect      item
                           drawButton "Level Editor"  screenSize 0      MmiLevelEditor  item
                           drawButton "Options"       screenSize 60     MmiOptions      item
                           drawButton "Exit"          screenSize 120    MmiExit         item

                  ScnGame _ -> do
                     clearBackground skyColor

                  ScnLevelEditorSelect (SceneLevelEditorSelect lvls sel) -> do
                     clearBackground black
                     drawButton "New level..." screenSize (-210) 0 sel

                     if null lvls then
                        drawTextCentered "No levels found" screenSize 0 48 white
                     else
                        forM_ (zip [1..] lvls) $
                           \(i, LevelDescr name) ->  
                              drawButton name screenSize (-180 + i * 60) i sel 

                  ScnLevelEditor _ -> do
                     clearBackground black
                     mode3D (camera s) $ do
                        drawGrid 10 1.0
                        drawCube (Vector3 0 0 0) 2 2 2 red

                  ScnNewLevel (SceneNewLevel name (Dims w h d) item) -> do
                     clearBackground black
                     drawTextCentered "Create new level" screenSize (-240) 48 white

                     drawTextCentered "Name" screenSize (-160) 48 white
                     drawTextInput name screenSize 600 (-100) SNLEditLabel item

                     drawTextCentered "Width" screenSize (-40) 48 white
                     drawTextInput (showOrEmpty w) screenSize 150 20 SNLEditDimW item

                     drawTextCentered "Width" screenSize 80 48 white
                     drawTextInput (showOrEmpty h) screenSize 150 140 SNLEditDimH item

                     drawTextCentered "Width" screenSize 200 48 white
                     drawTextInput (showOrEmpty d) screenSize 150 260 SNLEditDimD item

                     drawButton "Submit" screenSize 320 SNLSubmit item

                  ScnExit -> do
                     closeAudioDevice (Just win)
                     return ()
                     
                  ScnOptions _ -> do
                     clearBackground red

                  _ -> todo' "draw another scenes"

               when (showFps s) $ drawFPS 0 0

            updateState s

loadLevels :: IO [LevelDescr]
loadLevels = do
   createDirectoryIfMissing False "levels"
   files <- getDirectoryContents "levels"
   let lvlFiles = filter (\f -> takeExtension f == ".lvl") files
   return $ map LevelDescr lvlFiles

whileWindowOpen :: State -> (State -> IO State) -> IO ()
whileWindowOpen state f = do
   newState    <- f state
   shouldClose <- windowShouldClose

   if shouldClose || isExitState newState
      then return ()
      else whileWindowOpen newState f

-- main :: IO ()
-- main = do
--    let filename = "test.lvl"

--    let dims = Dims 2 2 2
--    ml <- newLevel dims
--    setBlock ml (0, 0, 0) BFinish
--    setBlock ml (1, 0, 0) BPunch
--    setBlock ml (0, 1, 0) $ BHeight DirFront 3
--    lvl <- freezeLevel ml
--    print lvl
--    BS.writeFile filename $ serializeLevel lvl

--    loaded <- BS.readFile filename
--    let lvlDe = deserializeLevel loaded
--    case lvlDe of
--       Left err -> putStrLn $ errorBundlePretty err
--       Right lvl2 -> print lvl2

--    return ()