module Constants where

import Raylib.Types

targetFps :: Int
targetFps = 60

title :: String
title = "T.R.A.N.S.M.I.S.S.I.O.N"

skyColor :: Color
skyColor = Color 171 214 255 255

windowWidth :: Int
windowWidth = 1024

windowHeight :: Int
windowHeight = 768

levelMaxSize :: Integral a => a
levelMaxSize = 64