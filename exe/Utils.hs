module Utils where

import Paths_transmission (getDataFileName)
import Raylib.Util (managed, WindowResources)
import Raylib.Types
import Raylib.Core.Models (loadModel, setMaterialTexture, loadMaterialDefault, drawCubeWires)
import Raylib.Core.Text (drawText, measureText)
import Control.Exception (try, IOException)
import Raylib.Core.Textures
import Raylib.Util.Colors
import Raylib.Core.Shapes (drawRectangleLinesEx)
import Raylib.Core (isKeyPressed, isKeyPressedRepeat)
import Raylib.Util.RLGL (rlSetLineWidth)

rotToBouncing :: Float -> Float
rotToBouncing rot = sin ((rot * pi) / 180) / 4

tryReadFile :: String -> IO (Maybe String)
tryReadFile file = do
   result <- try $ readFile file :: IO (Either IOException String)
   return $ case result of
      Left _  -> Nothing
      Right v -> Just v

loadTexturedModel :: WindowResources -> FilePath -> FilePath -> IO Model
loadTexturedModel w mdlP txtP = do
   mdl  <- managed w $ loadModel =<< getDataFileName mdlP
   txt  <- loadTexture =<< getDataFileName txtP
   mat  <- managed w loadMaterialDefault
   mat2 <- setMaterialTexture mat MaterialMapAlbedo txt

   return mdl { model'materials = [mat2] }

loadMainMaterial :: WindowResources -> IO Material
loadMainMaterial w = do
   txt <- loadTexture =<< getDataFileName "assets/solid.png"
   mat <- managed w loadMaterialDefault
   setMaterialTexture mat MaterialMapAlbedo txt

loadCrosshair :: WindowResources -> IO Texture
loadCrosshair w = managed w $ loadTexture "assets/crosshair101.png" >>= 
   \t -> setTextureFilter t TextureFilterBilinear

drawCrosshair :: (Int, Int) -> Texture -> IO ()
drawCrosshair (w, h) crosshair = do
   let x = (fromIntegral w - fromIntegral (texture'width crosshair)) / 2
   let y = (fromIntegral h - fromIntegral (texture'height crosshair)) / 2
   drawTextureEx crosshair (Vector2 x y) 0 0.75 white

drawButton :: Eq a
   => String
   -> (Int, Int)
   -> Int
   -> a
   -> a
   -> IO ()
drawButton label (width, height) offset assigned current = do
   let fontSize = 48
   let color = if assigned == current
         then green
         else red

   measure <- measureText label fontSize

   drawText label ((width - measure) `div` 2) (height `div` 2 + offset) fontSize color

drawThickCube :: Integral a => (a, a, a) -> Float -> Color -> IO ()
drawThickCube (x, y, z) lineWidth color = do
   rlSetLineWidth lineWidth
   drawCubeWires (Vector3 (fromIntegral x + 0.5) (fromIntegral y + 0.5) (fromIntegral z + 0.5)) 1.1 1.1 1.1 color

drawTextInput :: Eq a
   => String
   -> (Int, Int)
   -> Int
   -> Int
   -> a
   -> a
   -> IO ()
drawTextInput inputText (w, h) width offset assigned current = do
   let fontSize = 48
   let lineColor = if assigned == current
         then green
         else red
   let textColor = white
   let x = fromIntegral $ (w - width) `div` 2
   let y = fromIntegral $ (h - 60) `div` 2 + offset

   drawRectangleLinesEx (Rectangle x y (fromIntegral width) 60) 2 lineColor
   drawText inputText (truncate x + 10) (h `div` 2 + offset - 20) fontSize textColor

drawTextCentered :: String -> (Int, Int) -> Int -> Int -> Color -> IO ()
drawTextCentered label (width, height) offsetY fontSize color = do
   measure <- measureText label fontSize
   drawText label ((width - measure) `div` 2) (((height - fontSize) `div` 2) + offsetY) fontSize color

todo__ :: String -> a
todo__ err = error $ "TODO: " ++ err

unreachable' :: a
unreachable' = error "This part of code must be unreachable"

eitherToMaybe :: Either e t -> Maybe t
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right a) = Just a

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

readBool :: String -> Maybe Bool
readBool "True"  = Just True
readBool "False" = Just False
readBool _       = Nothing

readInt :: String -> Maybe Int
readInt s = case reads s of
   [(n, "")] -> Just n
   _         -> Nothing

isKeyPressedMaybeRepeat :: KeyboardKey -> IO Bool
isKeyPressedMaybeRepeat k = (||) <$> isKeyPressed k <*> isKeyPressedRepeat k

showOrEmpty :: (Num a, Eq a, Show a) => a -> String
showOrEmpty 0 = ""
showOrEmpty a = show a