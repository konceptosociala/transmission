module Render.LevelEditorSelect where

import Control.Monad (forM_)
import Raylib.Core
import Raylib.Util.Colors

import Utils
import Scene.LevelEditor (LevelDescr (..), SceneLevelEditorSelect (..))

renderLevelEditorSelect :: SceneLevelEditorSelect -> (Int, Int) -> IO ()
renderLevelEditorSelect (SceneLevelEditorSelect lvls sel) screenSize = do
   clearBackground black
   drawButton "New level..." screenSize (-210) 0 sel

   if null lvls then
      drawTextCentered "No levels found" screenSize 0 48 white
   else
      -- // TODO: scroll if too many levels
      forM_ (zip [1..] lvls) $
         \(i, LevelDescr name) ->  
            drawButton name screenSize (-180 + i * 60) i sel 