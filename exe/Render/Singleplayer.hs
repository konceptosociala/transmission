module Render.Singleplayer where

import Control.Monad (forM_)
import Raylib.Core
import Raylib.Util.Colors

import Utils
import Scene.MainMenu (SceneSingleplayer(..))
import Scene.LevelEditor (LevelDescr(..))


renderSingleplayer :: SceneSingleplayer -> (Int, Int) -> IO ()
renderSingleplayer (SceneSingleplayer levels sel) (width, height) = do
   clearBackground black
   drawTextCentered "Select level:" (width, height) (-250) 48 white

   forM_ (zip [0..] levels) $
      \(i, LevelDescr name) ->
         drawButton name (width, height) (-100 + i * 60) i sel
