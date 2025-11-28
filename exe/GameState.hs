{-# LANGUAGE RecordWildCards #-}
module GameState where

import Control.Monad (when, unless)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Char (ord, chr)
import Data.Function

import Raylib.Core
import Raylib.Types
import Raylib.Core.Audio (playSound, isMusicStreamPlaying, stopMusicStream, playMusicStream)
import Raylib.Core.Camera (updateCamera)
import Raylib.Core.Models (getRayCollisionMesh, getRayCollisionQuad, uploadMesh)
import Raylib.Util.Math (matrixTranslate)

import Sounds
import Utils
import Constants 

import Level
import Level.Mesh (generateChunkMesh)
import Level.Manipulate (freezeLevel, serializeLevel, setBlock, newLevel)

import Scene.LevelEditor
import Scene.MainMenu
import Scene.Options
import Scene.Game

data State = State
   { camera :: Camera3D
   , optionsValue :: Options
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

initState :: Sounds -> Options -> State
initState sounds options = State
   { showFps = False
   , optionsValue = options
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
   currentScene_ <- case updatedScene of
         initial@(ScnMainMenu (SceneMainMenu item selected _ _)) ->
            if selected
               then case item of
                  MmiSingleplayer -> pure $ ScnSingleplayer SceneSingleplayer
                  MmiConnect      -> pure $ ScnConnect SceneConnect
                  MmiLevelEditor  -> do
                     loadedLevels <- loadLevels
                     pure $ ScnLevelEditorSelect $ SceneLevelEditorSelect loadedLevels 0

                  MmiOptions      -> pure $ ScnOptions (SceneOptions OptMusicVolume (optionsValue state) False)
                  MmiExit         -> pure ScnExit
               else pure initial
         other -> pure other

   let _camera = case currentScene_ of
         ScnLevelEditor scn -> leCam scn
         ScnGame _ -> todo__ "game camera"
         _ -> mainMenuCam

   -- Check if we need to apply options
   case currentScene_ of
      ScnOptions (SceneOptions _ opts True) -> do
         updateSounds (sounds state) opts
         setFullScreen (isFullscreen opts)
      _ ->
         pure ()

   return state
      { camera       = _camera
      , currentScene = currentScene_
      , showFps      = showFps_
      , optionsValue = optionsValue_
      }

updateScene :: Scene -> Sounds -> Options -> IO Scene
updateScene (ScnGame scene) sound' _               = updateSceneGame scene sound'
updateScene (ScnSingleplayer scene) sound' _       = updateSceneSingleplayer scene sound'
updateScene (ScnConnect scene) sound' _            = updateSceneConnect scene sound'
updateScene (ScnMainMenu scene) sound' _           = updateSceneMainMenu scene sound'
updateScene (ScnLevelEditor scene) sound' _        = updateSceneLevelEditor scene sound'
updateScene (ScnLevelEditorSelect scene) sound' _  = updateSceneLevelEditorSelect scene sound'
updateScene (ScnOptions scene) sound' opts         = updateSceneOptions scene sound' opts
updateScene (ScnNewLevel scene) sound' _           = updateSceneNewLevel scene sound'
updateScene ScnExit _ _                            = pure ScnExit

updateSceneGame :: SceneGame -> Sounds -> IO Scene
updateSceneGame game _ = pure $ ScnGame game

updateSceneSingleplayer :: SceneSingleplayer -> Sounds -> IO Scene
updateSceneSingleplayer = todo__ "update SceneSingleplayer"

updateSceneConnect :: SceneConnect -> Sounds -> IO Scene
updateSceneConnect = todo__ "update SceneConnect"

updateSceneMainMenu :: SceneMainMenu -> Sounds -> IO Scene
updateSceneMainMenu (SceneMainMenu item _ rot msgbox) sound' = do
   isMusic <- isMusicStreamPlaying $ mscMenuBg sound'

   unless isMusic
      $ playMusicStream $ mscMenuBg sound'

   case msgbox of
      Just msg -> do
         ok <- checkMsgBox

         return $ ScnMainMenu SceneMainMenu
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

         return $ ScnMainMenu SceneMainMenu {..}

updateSceneLevelEditor :: SceneLevelEditor -> Sounds -> IO Scene
updateSceneLevelEditor (SceneLevelEditor cam lvl (LevelDescr name) meshes _ mode) sound' = do
   isMusic <- isMusicStreamPlaying $ mscMenuBg sound'

   when isMusic
      $ stopMusicStream $ mscMenuBg sound'
   
   esc   <- isKeyPressed KeyEscape
   shift <- (||) <$> isKeyDown KeyLeftShift <*> isKeyDown KeyRightShift
   f     <- isKeyPressed KeyF

   mouseLeft   <- isMouseButtonPressed MouseButtonLeft
   mouseRight  <- isMouseButtonPressed MouseButtonRight

   if not esc then do
      -- Save level if Shift+F is pressed
      when (shift && f) $ do
         playSound $ sndClick sound'

         dat <- serializeLevel <$> freezeLevel lvl
         BS.writeFile ("levels/"++name) dat

      leCam_ <- updateCamera cam CameraModeFree

      let screenCenter = Vector2 (fromIntegral windowWidth / 2.0) (fromIntegral windowHeight / 2.0)
      ray <- getScreenToWorldRay screenCenter leCam_

      let Dims w _ d = mlvlDims lvl
      let offsetX = negate (fromIntegral (w `div` 2))
      let offsetZ = negate (fromIntegral (d `div` 2))
      
      -- Check collision against ALL chunk meshes with their proper transforms
      let chunkCollisions = 
            [ let chunkOffsetX = fromIntegral cx * fromIntegral chunkSize
                  chunkOffsetY = fromIntegral cy * fromIntegral chunkSize
                  chunkOffsetZ = fromIntegral cz * fromIntegral chunkSize
                  matrix = matrixTranslate (offsetX + chunkOffsetX) chunkOffsetY (offsetZ + chunkOffsetZ)
              in getRayCollisionMesh ray mesh matrix
            | ((cx, cy, cz), mesh) <- HM.toList meshes
            ]
      
      let rc = case filter rayCollision'hit chunkCollisions of
            [] -> RayCollision False 0 (Vector3 0 0 0) (Vector3 0 0 0)
            hits -> minimumBy (comparing rayCollision'distance) hits
            
      let floorSize = fromIntegral (max w d)
      let frc = getRayCollisionQuad ray
            (Vector3 (negate floorSize/2) 0 (floorSize/2))
            (Vector3 (floorSize/2) 0 (floorSize/2))
            (Vector3 (floorSize/2) 0 (negate floorSize/2))
            (Vector3 (negate floorSize/2) 0 (negate floorSize/2))
      
      let collision = case (rayCollision'hit rc, rayCollision'hit frc) of
            (True, True)   -> if rayCollision'distance rc < rayCollision'distance frc then rc else frc
            (True, False)  -> rc
            (False, True)  -> frc
            (False, False) -> rc
      
      let selectedInside
            | rayCollision'hit collision = 
               let hitPoint = rayCollision'point collision
                   normal = rayCollision'normal collision
                   offset = 0.01
                   blockPoint = Vector3
                     (vector3'x hitPoint - vector3'x normal * offset)
                     (vector3'y hitPoint - vector3'y normal * offset)
                     (vector3'z hitPoint - vector3'z normal * offset)
               in Just
                  ( floor (vector3'x blockPoint)
                  , floor (vector3'y blockPoint)
                  , floor (vector3'z blockPoint)
                  )
            | otherwise = Nothing

      let selectedOutside :: Maybe (Int, Int, Int)
            | rayCollision'hit collision = 
               let hitPoint = rayCollision'point collision
                   normal = rayCollision'normal collision
                   offset = 0.01
                   blockPoint = Vector3
                     (vector3'x hitPoint + vector3'x normal * offset)
                     (vector3'y hitPoint + vector3'y normal * offset)
                     (vector3'z hitPoint + vector3'z normal * offset)
               in Just
                  ( floor (vector3'x blockPoint)
                  , floor (vector3'y blockPoint)
                  , floor (vector3'z blockPoint)
                  )
            | otherwise = Nothing

      newMeshes <- case (selectedInside, selectedOutside) of
         (Just (xi, yi, zi), Just (xo, yo, zo)) -> do
            let localXi = xi + (fromIntegral w `div` 2)
            let localZi = zi + (fromIntegral d `div` 2)

            let localXo = xo + (fromIntegral w `div` 2)
            let localZo = zo + (fromIntegral d `div` 2)

            if mouseLeft then do
               setBlock lvl (fromIntegral localXo, fromIntegral yo, fromIntegral localZo) BSolid
               
               -- Regenerate affected chunk only (use local coordinates)
               let chunkCoord = worldToChunkCoord (fromIntegral localXo, fromIntegral yo, fromIntegral localZo)
               m <- generateChunkMesh lvl chunkSize chunkCoord
               mesh <- uploadMesh m False
               return $ HM.insert chunkCoord mesh meshes

            else if mouseRight then do
               setBlock lvl (fromIntegral localXi, fromIntegral yi, fromIntegral localZi) BEmpty
               
               -- Regenerate affected chunk only (use local coordinates)
               let chunkCoord = worldToChunkCoord (fromIntegral localXi, fromIntegral yi, fromIntegral localZi)
               m <- generateChunkMesh lvl chunkSize chunkCoord
               mesh <- uploadMesh m False
               return $ HM.insert chunkCoord mesh meshes
               
            else
               return meshes

         _ -> return meshes

      return $ ScnLevelEditor (SceneLevelEditor leCam_ lvl (LevelDescr name) newMeshes selectedInside mode)
   else
      return $ ScnMainMenu mkMainMenu

updateSceneLevelEditorSelect :: SceneLevelEditorSelect -> Sounds -> IO Scene
updateSceneLevelEditorSelect (SceneLevelEditorSelect lvls i) sound' = do
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
               Just lvl -> ScnLevelEditor <$> mkSceneLevelEditor lvl descr
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

updateSceneOptions :: SceneOptions -> Sounds -> Options -> IO Scene
updateSceneOptions (SceneOptions sel _ finished) sound' opts = do
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

updateSceneNewLevel :: SceneNewLevel -> Sounds -> IO Scene
updateSceneNewLevel initial@(SceneNewLevel name dims@(Dims w h d) item) sound' = do
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

            ScnLevelEditor <$> mkSceneLevelEditor level descr
         else do
            playSound $ sndError sound'
            return $ ScnNewLevel initial
      else
         let nlSelectedItem
               | up        = prevSNLItem item
               | down      = nextSNLItem item
               | otherwise = item

             nlDims = case item of
               SNLEditDimW -> dims { dimsW = min levelMaxSize $ max 0 $ adjust w }
               SNLEditDimH -> dims { dimsH = min levelMaxSize $ max 0 $ adjust h }
               SNLEditDimD -> dims { dimsD = min levelMaxSize $ max 0 $ adjust d }
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