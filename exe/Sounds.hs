{-# LANGUAGE RecordWildCards #-}
module Sounds where

import Raylib.Types (Sound, Music)
import Raylib.Core.Audio (loadSound, loadMusicStream, setSoundVolume, setMusicVolume)
import Paths_transmission (getDataFileName)
import Raylib.Util (WindowResources, managed)
import Options (Options (Options))

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

updateSounds :: Sounds -> Options -> IO ()
updateSounds sounds (Options musicVolume soundVolume _) = do
   setSoundVolume (sndClick sounds) (fromIntegral soundVolume / 100)
   setSoundVolume (sndHover sounds) (fromIntegral soundVolume / 100)
   setSoundVolume (sndError sounds) (fromIntegral soundVolume / 100)
   setMusicVolume (mscMenuBg sounds) (fromIntegral musicVolume / 100)
