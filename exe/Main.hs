module Main where

import Raylib.Util (withWindow, mode3D, drawing)
import Raylib.Core
import Raylib.Types
import Raylib.Core.Models
import Raylib.Core.Text (drawFPS, drawText)
import Control.Monad (when, forM_)
import GameState (Scene(..), State (showFps, currentScene, camera), initState, updateState, isExitState)
import Utils
import MainMenu (SceneMainMenu(SceneMainMenu), MainMenuItem (..), drawMsgBox, setFullScreen)
import Raylib.Util.Colors
import LevelEditor (LevelDescr (..), SceneLevelEditorSelect (SceneLevelEditorSelect), SceneNewLevel (SceneNewLevel), SNLItem (..), SceneLevelEditor (SceneLevelEditor))
import Level (Dims(Dims), MLevel (MLevel))
import Sounds (loadSounds, Sounds (mscMenuBg), updateSounds)
import Raylib.Core.Audio (initAudioDevice, closeAudioDevice, updateMusicStream, isMusicStreamPlaying)
import Options (loadOrCreateOptions, SceneOptions (SceneOptions), OptionsItem (..), Options (..))
import Constants
import Raylib.Core.Shapes (drawRectangle)
import qualified Data.HashMap.Strict as HM
import Raylib.Util.Math (matrixTranslate)

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

      -- loadedLevels <- loadLevels
      setFullScreen (isFullscreen options)

      whileWindowOpen (initState sounds options) $
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

                  ScnOptions (SceneOptions sel opts _) -> do
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

                  ScnLevelEditorSelect (SceneLevelEditorSelect lvls sel) -> do
                     clearBackground black
                     drawButton "New level..." screenSize (-210) 0 sel

                     if null lvls then
                        drawTextCentered "No levels found" screenSize 0 48 white
                     else
                        -- // TODO: scroll if too many levels
                        forM_ (zip [1..] lvls) $
                           \(i, LevelDescr name) ->  
                              drawButton name screenSize (-180 + i * 60) i sel 

                  ScnLevelEditor (SceneLevelEditor _ (MLevel (Dims w h d) _) (LevelDescr name) meshes sel mode) -> do
                     clearBackground black
                     mode3D (camera s) $ do
                        drawGrid (levelMaxSize + 3) 1.0
                        let offsetX = negate (fromIntegral (w `div` 2))
                        let offsetZ = negate (fromIntegral (d `div` 2))

                        forM_ (HM.toList meshes) $ \((cx, cy, cz), mesh) -> do
                           let chunkOffsetX = fromIntegral cx * fromIntegral chunkSize
                           let chunkOffsetY = fromIntegral cy * fromIntegral chunkSize
                           let chunkOffsetZ = fromIntegral cz * fromIntegral chunkSize
                           let matrix = matrixTranslate (offsetX + chunkOffsetX) chunkOffsetY (offsetZ + chunkOffsetZ)
                           drawMesh mesh mainMat matrix

                        case sel of
                           Just (x, y, z) -> 
                              drawThickCube (x, y, z) 5.0 green

                           Nothing -> 
                              return ()

                     drawRectangle 0 0 300 200 (Color 0 0 0 160)
                     drawText name 20 20 32 white
                     drawText ("Size: " ++ show w ++ " " ++ show h ++ " " ++ show d) 20 70 20 lightGray
                     drawText "Escape - Back to menu" 20 100 20 lightGray
                     drawText "Shift+F - Save level" 20 130 20 lightGray
                     drawText ("Current mode: " ++ show mode) 20 160 20 lightGray
                     drawCrosshair screenSize crosshair
   
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

                  _ -> todo__ "draw another scenes"

               when (showFps s) $ drawFPS 0 0

            updateState s

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