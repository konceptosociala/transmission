module Constants where

import Raylib.Types
import Data.Binary (Word16)

targetFps :: Int
targetFps = 60

chunkSize :: Word16
chunkSize = 16

title :: String
title = "T.R.A.N.S.M.I.S.S.I.O.N"

skyColor :: Color
skyColor = Color 171 214 255 255

windowWidth :: Int
windowWidth = 1024

windowHeight :: Int
windowHeight = 768

levelMaxSize :: Integral a => a
levelMaxSize = 256

playerSize :: Vector3
playerSize = Vector3 0.6 1.8 0.6

physTimeStep :: Float
physTimeStep = 1 / 60

gravity :: Float
gravity = 9.81

moveSpeed :: Float
moveSpeed = 5.0