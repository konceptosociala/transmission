module Options where

import Utils (tryReadFile, todo')

newtype SceneOptions = SceneOptions
   { optSelectedItem :: OptionsItem
   }

data Options = Options
   { musicVolume :: Int
   , soundVolume :: Int
   , isFullscreen :: Bool
   }

data OptionsItem
   = OptMusicVolume
   | OptSoundVolume
   | OptFullscreen
   | OptSave
   | OptCancel
   deriving Eq

openConfig :: IO Options
openConfig = do
   _ <- tryReadFile "settings.ini"

   todo' "open config"

saveConfig :: Options -> IO ()
saveConfig _ = todo' "save config"