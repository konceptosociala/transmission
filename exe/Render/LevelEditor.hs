module Render.LevelEditor where

import qualified Data.HashMap.Strict as HM
import Control.Monad (forM_)

import Raylib.Util (mode3D)
import Raylib.Core
import Raylib.Types
import Raylib.Core.Models
import Raylib.Core.Text (drawText)
import Raylib.Util.Colors
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Util.Math (matrixTranslate)

import Utils
import Constants

import Scene.LevelEditor (LevelDescr (..), SceneLevelEditor (..))
import Level (Dims(..), MLevel (..))

renderLevelEditor 
   :: SceneLevelEditor 
   -> (Int, Int) 
   -> Texture 
   -> Material
   -> Camera3D 
   -> IO ()
renderLevelEditor le screenSize crosshair mainMat camera = do
   let (Dims w h d) = mlvlDims (leCurrentLevel le)
   let (LevelDescr name) = leLevelDescr le

   clearBackground black
   mode3D camera $ do
      drawGrid (levelMaxSize + 3) 1.0
      let offsetX = negate (fromIntegral (w `div` 2))
      let offsetZ = negate (fromIntegral (d `div` 2))

      forM_ (HM.toList (leChunkMeshes le)) $ \((cx, cy, cz), mesh) -> do
         let chunkOffsetX = fromIntegral cx * fromIntegral chunkSize
         let chunkOffsetY = fromIntegral cy * fromIntegral chunkSize
         let chunkOffsetZ = fromIntegral cz * fromIntegral chunkSize
         let matrix = matrixTranslate (offsetX + chunkOffsetX) chunkOffsetY (offsetZ + chunkOffsetZ)
         drawMesh mesh mainMat matrix

      case leSelectedBlock le of
         Just (x, y, z) -> 
            drawThickCube (x, y, z) 5.0 green

         Nothing -> 
            return ()

   drawRectangle 0 0 300 200 (Color 0 0 0 160)
   drawText name 20 20 32 white
   drawText ("Size: " ++ show w ++ " " ++ show h ++ " " ++ show d) 20 70 20 lightGray
   drawText "Escape - Back to menu" 20 100 20 lightGray
   drawText "Shift+F - Save level" 20 130 20 lightGray
   drawText ("Current mode: " ++ show (leCurrentBlockType le)) 20 160 20 lightGray
   drawCrosshair screenSize crosshair