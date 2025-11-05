{-# LANGUAGE RecordWildCards #-}
module Sounds where

import Raylib.Types (Sound, Music)
import Raylib.Core.Audio (loadSound, loadMusicStream)
import Paths_transmission (getDataFileName)
import Raylib.Util (WindowResources, managed)

data Sounds = Sounds
   -- Sounds
   { sndClick :: Sound
   , sndHover :: Sound
   , sndError :: Sound
   -- Music
   , mscMenuBg :: Music
   }

loadSounds :: WindowResources -> IO Sounds
loadSounds w = do
   sndClick <- managed w $ loadSound =<< getDataFileName "assets/sounds/click.ogg"
   sndHover <- managed w $ loadSound =<< getDataFileName "assets/sounds/hover.ogg"
   sndError <- managed w $ loadSound =<< getDataFileName "assets/sounds/error.ogg"

   mscMenuBg <- managed w $ loadMusicStream =<< getDataFileName "assets/music/menuBg.ogg"

   return Sounds {..}