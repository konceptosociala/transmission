module Scene.MainMenu where

import Raylib.Types
import Utils (MsgBox)
import Scene.LevelEditor (LevelDescr)

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
   { spLevels :: [LevelDescr]
   , spSelected :: Int
   }

data SceneConnect = SceneConnect

data MainMenuItem
   = MmiSingleplayer
   | MmiConnect
   | MmiLevelEditor
   | MmiOptions
   | MmiExit
   deriving Eq

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