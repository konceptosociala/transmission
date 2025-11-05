{-# LANGUAGE OverloadedStrings #-}
module Options where

import qualified Data.HashMap.Strict as HM
import Utils (eitherToMaybe)
import Data.Ini
import Data.Text (pack)
import Raylib.Core (fileExists)

newtype SceneOptions = SceneOptions
   { optSelectedItem :: OptionsItem
   }

data Options = Options
   { musicVolume :: Int
   , soundVolume :: Int
   , isFullscreen :: Bool
   }

optsDefault :: Options
optsDefault = Options 100 100 True

optsToIni :: Options -> Ini
optsToIni (Options mVol sVol isFS) = Ini
   { iniSections = HM.fromList
      [  ("SETTINGS",
            [ ("mvol", pack $ show mVol)
            , ("svol", pack $ show sVol)
            , ("isfs", pack $ show isFS)
            ]
         )
      ]
   , iniGlobals = []
   }

optsFromIni :: Ini -> Maybe Options
optsFromIni ini = do
   let lookupField sect key = eitherToMaybe $ lookupValue sect key ini

   mvolT <- show <$> lookupField "SETTINGS" "mvol"
   svolT <- show <$> lookupField "SETTINGS" "svol"
   isfsT <- show <$> lookupField "SETTINGS" "isfs"

   return $ Options (read mvolT) (read svolT) (read isfsT)

data OptionsItem
   = OptMusicVolume
   | OptSoundVolume
   | OptFullscreen
   | OptSave
   | OptCancel
   deriving Eq

loadOrCreateOptions :: IO Options
loadOrCreateOptions = do
   let def = saveConfig optsDefault >> return optsDefault

   f <- fileExists "settings.ini"
   if not f then 
      def 
   else do
      file <- eitherToMaybe <$> readIniFile "settings.ini"
      case file of
         Just ini -> maybe def pure (optsFromIni ini)
         Nothing -> def

saveConfig :: Options -> IO ()
saveConfig opts = writeIniFile "settings.ini" (optsToIni opts)