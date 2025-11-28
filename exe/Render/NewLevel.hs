module Render.NewLevel where

import Raylib.Core
import Raylib.Util.Colors

import Level
import Utils
import Scene.LevelEditor

renderNewLevel :: SceneNewLevel -> (Int, Int) -> IO ()
renderNewLevel (SceneNewLevel name (Dims w h d) item) screenSize = do
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