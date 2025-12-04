module Render.Game where

import Control.Monad (forM_)
import qualified Data.HashMap.Strict as HM

import Raylib.Core
import Raylib.Core.Models
import Raylib.Util
import Raylib.Util.Math (matrixTranslate)
import Raylib.Types

import Scene.Game 

import Constants
import Level

renderGame :: SceneGame -> Material -> IO ()
renderGame game mainMat = do
   clearBackground skyColor
   let (Dims w _ d) = lvlDims (gmLevel game)

   mode3D (gmPlayerCam game) $ do
      let offsetX = negate (fromIntegral (w `div` 2))
      let offsetZ = negate (fromIntegral (d `div` 2))

      forM_ (HM.toList (gmChunkMeshes game)) $ \((cx, cy, cz), mesh) -> do
         let chunkOffsetX = fromIntegral cx * fromIntegral chunkSize
         let chunkOffsetY = fromIntegral cy * fromIntegral chunkSize
         let chunkOffsetZ = fromIntegral cz * fromIntegral chunkSize
         let matrix = matrixTranslate (offsetX + chunkOffsetX) chunkOffsetY (offsetZ + chunkOffsetZ)
         drawMesh mesh mainMat matrix