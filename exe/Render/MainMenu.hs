module Render.MainMenu where

import Raylib.Util (mode3D)
import Raylib.Core
import Raylib.Types
import Raylib.Core.Models
import Raylib.Util.Colors

import Utils
import Scene.MainMenu

renderMainMenu 
   :: SceneMainMenu 
   -> (Int, Int) 
   -> Model
   -> Camera3D
   -> IO ()
renderMainMenu (SceneMainMenu item _ rot msgbox) screenSize logo camera = do
   clearBackground black
   case msgbox of
      Just mb -> drawMsgBox screenSize mb
      Nothing -> do
         mode3D camera $
            drawModelEx logo (Vector3 0 2 0) (Vector3 0 1 0) rot 0.5 white

         drawButton "Singleplayer"  screenSize (-120) MmiSingleplayer item
         drawButton "Connect..."    screenSize (-60)  MmiConnect      item
         drawButton "Level Editor"  screenSize 0      MmiLevelEditor  item
         drawButton "Options"       screenSize 60     MmiOptions      item
         drawButton "Exit"          screenSize 120    MmiExit         item