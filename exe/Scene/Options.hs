{-# LANGUAGE OverloadedStrings #-}
module Scene.Options where

import qualified Data.HashMap.Strict as HM
import Data.Ini
import Data.Text (pack, unpack)

import Raylib.Core (fileExists)

import Utils (eitherToMaybe, readInt, readBool)

data SceneOptions = SceneOptions
   { optSelectedItem :: OptionsItem
   , optInnerOpts :: Options
   , optFinished :: Bool
   }

data Options = Options
   { musicVolume :: Int
   , soundVolume :: Int
   , isFullscreen :: Bool
   , playerNickname :: String
   } deriving (Eq, Show)

optsDefault :: Options
optsDefault = Options 100 100 True "unnamed"
optsToIni :: Options -> Ini
optsToIni (Options mVol sVol isFS nick) = Ini
   { iniSections = HM.fromList
      [  ("SETTINGS",
            [ ("mvol", pack $ show mVol)
            , ("svol", pack $ show sVol)
            , ("isfs", pack $ show isFS)
            , ("nick", pack nick)
            ]
         )
      ]
   , iniGlobals = []
   }

optsFromIni :: Ini -> Maybe Options
optsFromIni ini = do
   let lookupField sect key = eitherToMaybe $ lookupValue sect key ini

   mvolT <- lookupField "SETTINGS" "mvol"
   svolT <- lookupField "SETTINGS" "svol"
   isfsT <- lookupField "SETTINGS" "isfs"
   nickT <- lookupField "SETTINGS" "nick"

   mvol <- readInt (unpack mvolT)
   svol <- readInt (unpack svolT)
   isfs <- readBool (unpack isfsT)
   let nick = unpack nickT

   if [0..100] `contains` mvol && [0..100] `contains` svol then
      Just (Options mvol svol isfs nick)
   else
      Nothing

   where
      contains range x = x >= minimum range && x <= maximum range

data OptionsItem
   = OptMusicVolume
   | OptSoundVolume
   | OptFullscreen
   | OptPlayerNickname
   | OptSave
   | OptCancel
   deriving Eq

prevOptionsItem :: OptionsItem -> OptionsItem
prevOptionsItem OptMusicVolume  = OptCancel
prevOptionsItem OptSoundVolume  = OptMusicVolume
prevOptionsItem OptFullscreen   = OptSoundVolume
prevOptionsItem OptPlayerNickname = OptFullscreen
prevOptionsItem OptSave         = OptPlayerNickname
prevOptionsItem OptCancel       = OptSave

nextOptionsItem :: OptionsItem -> OptionsItem
nextOptionsItem OptMusicVolume  = OptSoundVolume
nextOptionsItem OptSoundVolume  = OptFullscreen
nextOptionsItem OptFullscreen   = OptPlayerNickname
nextOptionsItem OptPlayerNickname = OptSave
nextOptionsItem OptSave         = OptCancel
nextOptionsItem OptCancel       = OptMusicVolume

loadOrCreateOptions :: IO Options
loadOrCreateOptions = do
   f <- fileExists "settings.ini"
   if not f then
      def
   else do
      file <- eitherToMaybe <$> readIniFile "settings.ini"
      case file of
         Just ini -> maybe def pure (optsFromIni ini)
         Nothing -> def

      where def = saveConfig optsDefault >> return optsDefault

saveConfig :: Options -> IO ()
saveConfig opts = writeIniFile "settings.ini" (optsToIni opts)