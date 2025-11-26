{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
module GameState where

import Raylib.Core (isKeyPressed, getCharPressed, isKeyDown)
import Raylib.Types (Camera3D(..), pattern Vector3, CameraProjection (CameraPerspective), KeyboardKey (..))
import MainMenu
import Options
import Game
import Utils (todo__, unreachable', safeInit, isKeyPressedMaybeRepeat)
import LevelEditor
import Data.Function
import Level (Dims(..))
import Data.Char (ord, chr)
import Level.Manipulate (newLevel, freezeLevel, serializeLevel)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Sounds
import Raylib.Core.Audio (playSound)

data State = State
   { camera :: Camera3D
   , optionsValue :: Options
   , loadedLevels :: [LevelDescr]
   , currentScene :: Scene
   , showFps :: Bool
   , sounds :: Sounds
   }

data Scene
   = ScnMainMenu SceneMainMenu
   | ScnOptions SceneOptions
   | ScnSingleplayer SceneSingleplayer
   | ScnConnect SceneConnect
   | ScnGame SceneGame
   | ScnLevelEditor SceneLevelEditor
   | ScnNewLevel SceneNewLevel
   | ScnLevelEditorSelect SceneLevelEditorSelect
   | ScnExit

isExitState :: State -> Bool
isExitState state =
   case currentScene state of
      ScnExit -> True
      _       -> False

initState :: [LevelDescr] -> Sounds -> Options -> State
initState loadedLevels sounds options = State
   { showFps = False
   , optionsValue = options
   , loadedLevels = loadedLevels
   , camera = Camera3D
      { camera3D'position = Vector3 3 1 0
      , camera3D'target = Vector3 0 1 0
      , camera3D'up = Vector3 0 1 0
      , camera3D'fovy = 70
      , camera3D'projection = CameraPerspective
      }
   , currentScene = ScnMainMenu mkMainMenu
   , sounds = sounds
   }

updateState :: State -> IO State
updateState state = do
   f3 <- isKeyPressed KeyF3

   let showFps_ = if f3 then not previous else previous
         where previous = showFps state

   updatedScene <- updateScene (currentScene state) (sounds state) (optionsValue state)

   -- Get updated options if changed
   let optionsValue_ = case updatedScene of
         (ScnOptions (SceneOptions _ opts _)) -> opts
         _ -> optionsValue state

   -- Open scene from main menu
   let currentScene_ = case updatedScene of
         initial@(ScnMainMenu (SceneMainMenu item selected _ _)) ->
            if selected
               then case item of
                  MmiSingleplayer -> ScnSingleplayer SceneSingleplayer
                  MmiConnect      -> ScnConnect SceneConnect
                  MmiLevelEditor  -> ScnLevelEditorSelect $ SceneLevelEditorSelect (loadedLevels state) 0
                  MmiOptions      -> ScnOptions (SceneOptions OptMusicVolume (optionsValue state) False)
                  MmiExit         -> ScnExit
               else initial
         other -> other

   -- Check if we need to apply options
   case currentScene_ of
      ScnOptions (SceneOptions _ opts True) -> do
         updateSounds (sounds state) opts
         setFullScreen (isFullscreen opts)
      _ -> 
         pure ()

   return state
      { camera       = getCamera currentScene_
      , currentScene = currentScene_
      , showFps      = showFps_
      , optionsValue = optionsValue_
      }

updateScene :: Scene -> Sounds -> Options -> IO Scene
updateScene (ScnGame _) _ _ = pure $ ScnGame SceneGame

updateScene (ScnMainMenu mainMenu) sound' _ = ScnMainMenu <$> updateMainMenu mainMenu sound'

updateScene (ScnLevelEditor editor@(SceneLevelEditor _cam lvl (LevelDescr name))) sound' _ = do
   esc  <- isKeyPressed KeyEscape
   ctrl <- isKeyDown KeyLeftControl
   s    <- isKeyPressed KeyS

   if not esc then do
      when (ctrl && s) $ do
         playSound $ sndClick sound'

         dat <- serializeLevel <$> freezeLevel lvl
         BS.writeFile ("levels/"++name) dat

      return $ ScnLevelEditor editor
   else
      return $ ScnMainMenu mkMainMenu

updateScene (ScnLevelEditorSelect (SceneLevelEditorSelect lvls i)) sound' _ = do
   esc   <- isKeyPressed KeyEscape
   down  <- isKeyPressed KeyDown
   up    <- isKeyPressed KeyUp
   enter <- isKeyPressed KeyEnter

   when (down || up)
      $ playSound $ sndHover sound'

   when enter
      $ playSound $ sndClick sound'

   if esc then
      return $ ScnMainMenu mkMainMenu
   else if enter then
      case compare i 0 of
         EQ -> return $ ScnNewLevel $ SceneNewLevel "new_level" (Dims 32 32 32) SNLEditLabel
         GT -> do
            let descr@(LevelDescr name) = lvls !! (i-1)
            lvlLoaded <- loadMLevel descr

            case lvlLoaded of
               Just lvl -> return $ ScnLevelEditor $ mkSceneLevelEditor lvl descr
               Nothing  -> return $ ScnMainMenu $
                  mkMainMenu
                     & withMsgBox (MsgBox $ "Error loading level `"++name++"`")

         LT -> unreachable'
   else
      let i_
            | up        = max (i - 1) 0
            | down      = min (i + 1) (length lvls)
            | otherwise = i

      in return $ ScnLevelEditorSelect $ SceneLevelEditorSelect lvls i_

updateScene (ScnOptions (SceneOptions sel _ finished)) sound' opts = do
   esc <- isKeyPressed KeyEscape

   if finished || esc then
      return $ ScnMainMenu mkMainMenu
   else do
      down  <- isKeyPressed KeyDown
      up    <- isKeyPressed KeyUp
      right <- isKeyPressed KeyRight
      left  <- isKeyPressed KeyLeft
      enter <- isKeyPressed KeyEnter

      when (down || up || right || left)
         $ playSound $ sndHover sound'

      let selectedItem_
            | up        = prevOptionsItem sel
            | down      = nextOptionsItem sel
            | otherwise = sel

      let options_ = case selectedItem_ of
            OptMusicVolume
               | right     -> opts { musicVolume = min 100 (musicVolume opts + 10) }
               | left      -> opts { musicVolume = max 0   (musicVolume opts - 10) }
               | otherwise -> opts

            OptSoundVolume
               | right     -> opts { soundVolume = min 100 (soundVolume opts + 10) }
               | left      -> opts { soundVolume = max 0   (soundVolume opts - 10) }
               | otherwise -> opts

            OptFullscreen
               | enter     -> opts { isFullscreen = not (isFullscreen opts) }
               | otherwise -> opts

            _ -> opts

      case (enter, selectedItem_) of
         (True, OptSave) -> do
            playSound $ sndClick sound'
            saveConfig options_
            return $ ScnOptions $ SceneOptions selectedItem_ options_ True

         (True, OptCancel) -> do
            playSound $ sndClick sound'
            oldOptions_ <- loadOrCreateOptions
            return $ ScnOptions $ SceneOptions selectedItem_ oldOptions_ True

         _ -> return $ ScnOptions $ SceneOptions selectedItem_ options_ False

updateScene (ScnNewLevel initial@(SceneNewLevel name dims@(Dims w h d) item)) sound' _ = do
   esc <- isKeyPressed KeyEscape

   if esc then
      return $ ScnMainMenu mkMainMenu
   else do
      down      <- isKeyPressed KeyDown
      up        <- isKeyPressed KeyUp
      backspace <- isKeyPressedMaybeRepeat KeyBackspace
      submit    <- isKeyPressed KeyEnter
      key       <- getCharPressed

      when (down || up)
         $ playSound $ sndHover sound'

      if submit && item == SNLSubmit then
         if notElem 0 [w, h, d] && name /= "" then do
            playSound $ sndClick sound'

            let descr = LevelDescr (name++".lvl")
            level <- newLevel dims

            return $ ScnLevelEditor $ mkSceneLevelEditor level descr
         else do
            playSound $ sndError sound'
            return $ ScnNewLevel initial
      else
         let nlSelectedItem
               | up        = prevSNLItem item
               | down      = nextSNLItem item
               | otherwise = item

             nlDims = case item of
               SNLEditDimW -> dims { dimsW = min 512 $ max 0 $ adjust w }
               SNLEditDimH -> dims { dimsH = min 512 $ max 0 $ adjust h }
               SNLEditDimD -> dims { dimsD = min 512 $ max 0 $ adjust d }
               _           -> dims
               where adjust x
                        | key >= ord '0'
                           && key <= ord '9' = x * 10 + fromIntegral (key - ord '0')
                        | backspace          = x `div` 10
                        | otherwise          = x


             nlLabel = take 20 $ case item of
               SNLEditLabel ->
                  if (key >= ord 'a' && key <= ord 'z')
                     || (key >= ord 'A' && key <= ord 'Z')
                     || key == ord '_'
                  then
                     name ++ [chr key]

                  else if backspace then
                     safeInit name

                  else
                     name

               _ -> name

         in return $ ScnNewLevel $ SceneNewLevel {..}

updateScene ScnExit _ _ = pure ScnExit

updateScene _ _ _ = todo__ "update other scenes"

getCamera :: Scene -> Camera3D
getCamera sc = case sc of
   ScnMainMenu _          -> mainMenuCam
   ScnOptions _           -> mainMenuCam
   ScnConnect _           -> mainMenuCam
   ScnExit                -> mainMenuCam
   ScnSingleplayer _      -> mainMenuCam
   ScnLevelEditorSelect _ -> mainMenuCam
   ScnNewLevel _          -> mainMenuCam
   ScnLevelEditor (SceneLevelEditor cam _ _) -> cam
   ScnGame _ -> todo__ "game camera"