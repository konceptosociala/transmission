{-# LANGUAGE RecordWildCards #-}
module Level.Mesh where

import Raylib.Types
import Data.Function ((&))
import Raylib.Core.Models (uploadMesh)
import Level (Dims (Dims), MLevel (mlvlDims), BlockType (BTSolid))
import Level.Manipulate
import Control.Monad.ST (RealWorld)
import Control.Monad (foldM)
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Unboxed as U
import Utils

data MeshBuilder s = MeshBuilder
   { mbVertices      :: MU.MVector s Float
   , mbTexcoords     :: MU.MVector s Float
   , mbNormals       :: MU.MVector s Float
   , mbVertexOffset  :: Int
   , mbTexOffset     :: Int
   , mbNormOffset    :: Int
   , mbTriangleCount :: Int
   }

freezeMesh :: MeshBuilder RealWorld -> IO Mesh
freezeMesh MeshBuilder{..} = do
    let vertCount = mbVertexOffset `div` 3

    vertices  <- U.unsafeFreeze (MU.slice 0 mbVertexOffset mbVertices)
    texcoords <- U.unsafeFreeze (MU.slice 0 mbTexOffset mbTexcoords)
    normals   <- U.unsafeFreeze (MU.slice 0 mbNormOffset mbNormals)

    return Mesh
      { mesh'vertexCount   = vertCount
      , mesh'triangleCount = mbTriangleCount
      , mesh'vertices      = floatsToVec3List vertices
      , mesh'texcoords     = floatsToVec2List texcoords
      , mesh'texcoords2    = Nothing
      , mesh'normals       = floatsToVec3List normals
      , mesh'tangents      = Nothing
      , mesh'colors        = Nothing
      , mesh'indices       = Nothing
      , mesh'animVertices  = Nothing
      , mesh'animNormals   = Nothing
      , mesh'boneIds       = Nothing
      , mesh'boneWeights   = Nothing
      , mesh'boneMatrices  = Nothing
      , mesh'boneCount     = 0
      , mesh'vaoId         = 0
      , mesh'vboId         = Nothing
      }

meshDefault :: Mesh
meshDefault = Mesh
   { mesh'vertexCount   = 0
   , mesh'triangleCount = 0
   , mesh'vertices      = []
   , mesh'texcoords     = []
   , mesh'texcoords2    = Nothing
   , mesh'normals       = []
   , mesh'tangents      = Nothing
   , mesh'colors        = Nothing
   , mesh'indices       = Nothing
   , mesh'animVertices  = Nothing
   , mesh'animNormals   = Nothing
   , mesh'boneIds       = Nothing
   , mesh'boneWeights   = Nothing
   , mesh'boneMatrices  = Nothing
   , mesh'boneCount     = 0
   , mesh'vaoId         = 0
   , mesh'vboId         = Nothing
   }

cube :: IO Mesh
cube =
   meshDefault
      & addFaceTop      (0 :: Int, 0, 0) BTSolid
      & addFaceBottom   (0 :: Int, 0, 0) BTSolid
      & addFaceFront    (0 :: Int, 0, 0) BTSolid
      & addFaceBack     (0 :: Int, 0, 0) BTSolid
      & addFaceLeft     (0 :: Int, 0, 0) BTSolid
      & addFaceRight    (0 :: Int, 0, 0) BTSolid
      & flip uploadMesh False

generateMesh :: MLevel RealWorld -> IO Mesh
generateMesh lvl = do
   let Dims w h d = mlvlDims lvl

   mesh <- foldM
      (\m (x, y, z) -> do
         currentBlock <- getBlockSolid lvl (x, y, z)
         
         case blockToType =<< currentBlock of
            Just ty -> do
               hasTop      <- if y == h - 1  then return False else hasBlock lvl (x, y + 1, z)
               hasBottom   <- if y == 0      then return False else hasBlock lvl (x, y - 1, z)
               hasFront    <- if z == d - 1  then return False else hasBlock lvl (x, y, z + 1)
               hasBack     <- if z == 0      then return False else hasBlock lvl (x, y, z - 1)
               hasLeft     <- if x == 0      then return False else hasBlock lvl (x - 1, y, z)
               hasRight    <- if x == w - 1  then return False else hasBlock lvl (x + 1, y, z)
               
               return $ m
                  & (if not hasTop     then addFaceTop      (x, y, z) ty else id)
                  & (if not hasBottom  then addFaceBottom   (x, y, z) ty else id)
                  & (if not hasFront   then addFaceFront    (x, y, z) ty else id)
                  & (if not hasBack    then addFaceBack     (x, y, z) ty else id)
                  & (if not hasLeft    then addFaceLeft     (x, y, z) ty else id)
                  & (if not hasRight   then addFaceRight    (x, y, z) ty else id)
            _ -> return m
      )
      meshDefault
      [ (x, y, z) | x <- [0 .. w - 1], y <- [0 .. h - 1], z <- [0 .. d - 1] ]

   uploadMesh mesh False

addFaceTop :: Integral a 
  => (a, a, a) 
  -> BlockType
  -> Mesh 
  -> Mesh
addFaceTop (xi, yi, zi) ty mesh =
   let normal = Vector3 0 1 0
       (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)

       texOffset = blockTypeCoords ty

       vertexData = mesh'vertices mesh ++
         [ Vector3 x (y + 1.0) z
         , Vector3 x (y + 1.0) (z + 1.0)
         , Vector3 (x + 1.0) (y + 1.0) z
         , Vector3 (x + 1.0) (y + 1.0) z
         , Vector3 x (y + 1.0) (z + 1.0)
         , Vector3 (x + 1.0) (y + 1.0) (z + 1.0)
         ]

       texCoords = mesh'texcoords mesh ++
         [ texOffset + Vector2 0.0 0.0
         , texOffset + Vector2 0.0 0.5
         , texOffset + Vector2 0.5 0.0
         , texOffset + Vector2 0.5 0.0
         , texOffset + Vector2 0.0 0.5
         , texOffset + Vector2 0.5 0.5
         ]

       normals = mesh'normals mesh ++ replicate 6 normal

   in mesh
      { mesh'vertices      = vertexData
      , mesh'normals       = normals
      , mesh'texcoords     = texCoords
      , mesh'triangleCount = mesh'triangleCount mesh + 2
      , mesh'vertexCount   = mesh'vertexCount mesh + 6
      }

addFaceBottom :: Integral a 
  => (a, a, a) 
  -> BlockType
  -> Mesh 
  -> Mesh
addFaceBottom (xi, yi, zi) ty mesh =
   let normal = Vector3 0 (-1) 0
       (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)

       texOffset = blockTypeCoords ty

       vertexData = mesh'vertices mesh ++
         [ Vector3 x y z
         , Vector3 (x + 1.0) y z
         , Vector3 x y (z + 1.0)
         , Vector3 (x + 1.0) y z
         , Vector3 (x + 1.0) y (z + 1.0)
         , Vector3 x y (z + 1.0)
         ]

       texCoords = mesh'texcoords mesh ++
         [ texOffset + Vector2 0.0 0.5
         , texOffset + Vector2 0.5 0.5
         , texOffset + Vector2 0.0 0.0
         , texOffset + Vector2 0.5 0.5
         , texOffset + Vector2 0.5 0.0
         , texOffset + Vector2 0.0 0.0
         ]

       normals = mesh'normals mesh ++ replicate 6 normal

   in mesh
      { mesh'vertices      = vertexData
      , mesh'normals       = normals
      , mesh'texcoords     = texCoords
      , mesh'triangleCount = mesh'triangleCount mesh + 2
      , mesh'vertexCount   = mesh'vertexCount mesh + 6
      }

addFaceFront :: Integral a 
  => (a, a, a) 
  -> BlockType
  -> Mesh 
  -> Mesh
addFaceFront (xi, yi, zi) ty mesh =
   let normal = Vector3 0 0 1
       (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)

       texOffset = blockTypeCoords ty

       vertexData = mesh'vertices mesh ++
         [ Vector3 x y (z + 1.0)
         , Vector3 (x + 1.0) y (z + 1.0)
         , Vector3 x (y + 1.0) (z + 1.0)
         , Vector3 (x + 1.0) y (z + 1.0)
         , Vector3 (x + 1.0) (y + 1.0) (z + 1.0)
         , Vector3 x (y + 1.0) (z + 1.0)
         ]

       texCoords = mesh'texcoords mesh ++
         [ texOffset + Vector2 0.5 0.0
         , texOffset + Vector2 0.0 0.0
         , texOffset + Vector2 0.5 0.5
         , texOffset + Vector2 0.0 0.0
         , texOffset + Vector2 0.0 0.5
         , texOffset + Vector2 0.5 0.5
         ]

       normals = mesh'normals mesh ++ replicate 6 normal

   in mesh
      { mesh'vertices      = vertexData
      , mesh'normals       = normals
      , mesh'texcoords     = texCoords
      , mesh'triangleCount = mesh'triangleCount mesh + 2
      , mesh'vertexCount   = mesh'vertexCount mesh + 6
      }

addFaceBack :: Integral a 
  => (a, a, a) 
  -> BlockType
  -> Mesh 
  -> Mesh
addFaceBack (xi, yi, zi) ty mesh =
   let normal = Vector3 0 0 (-1)
       (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)

       texOffset = blockTypeCoords ty

       vertexData = mesh'vertices mesh ++
         [ Vector3 x y z
         , Vector3 x (y + 1.0) z
         , Vector3 (x + 1.0) y z
         , Vector3 (x + 1.0) y z
         , Vector3 x (y + 1.0) z
         , Vector3 (x + 1.0) (y + 1.0) z
         ]

       texCoords = mesh'texcoords mesh ++
         [ texOffset + Vector2 0.0 0.0
         , texOffset + Vector2 0.0 0.5
         , texOffset + Vector2 0.5 0.0
         , texOffset + Vector2 0.5 0.0
         , texOffset + Vector2 0.0 0.5
         , texOffset + Vector2 0.5 0.5
         ]

       normals = mesh'normals mesh ++ replicate 6 normal

   in mesh
      { mesh'vertices      = vertexData
      , mesh'normals       = normals
      , mesh'texcoords     = texCoords
      , mesh'triangleCount = mesh'triangleCount mesh + 2
      , mesh'vertexCount   = mesh'vertexCount mesh + 6
      }

addFaceLeft :: Integral a 
  => (a, a, a) 
  -> BlockType
  -> Mesh 
  -> Mesh
addFaceLeft (xi, yi, zi) ty mesh =
   let normal = Vector3 (-1) 0 0
       (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)

       texOffset = blockTypeCoords ty

       vertexData = mesh'vertices mesh ++
         [ Vector3 x y z
         , Vector3 x y (z + 1.0)
         , Vector3 x (y + 1.0) z
         , Vector3 x (y + 1.0) z
         , Vector3 x y (z + 1.0)
         , Vector3 x (y + 1.0) (z + 1.0)
         ]

       texCoords = mesh'texcoords mesh ++
         [ texOffset + Vector2 0.5 0.0
         , texOffset + Vector2 0.0 0.0
         , texOffset + Vector2 0.5 0.5
         , texOffset + Vector2 0.5 0.5
         , texOffset + Vector2 0.0 0.0
         , texOffset + Vector2 0.0 0.5
         ]

       normals = mesh'normals mesh ++ replicate 6 normal

   in mesh
      { mesh'vertices      = vertexData
      , mesh'normals       = normals
      , mesh'texcoords     = texCoords
      , mesh'triangleCount = mesh'triangleCount mesh + 2
      , mesh'vertexCount   = mesh'vertexCount mesh + 6
      }

addFaceRight :: Integral a 
  => (a, a, a) 
  -> BlockType
  -> Mesh 
  -> Mesh
addFaceRight (xi, yi, zi) ty mesh =
   let normal = Vector3 1 0 0
       (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)

       texOffset = blockTypeCoords ty

       vertexData = mesh'vertices mesh ++
         [ Vector3 (x + 1.0) y z
         , Vector3 (x + 1.0) (y + 1.0) z
         , Vector3 (x + 1.0) y (z + 1.0)
         , Vector3 (x + 1.0) y (z + 1.0)
         , Vector3 (x + 1.0) (y + 1.0) z
         , Vector3 (x + 1.0) (y + 1.0) (z + 1.0)
         ]

       texCoords = mesh'texcoords mesh ++
         [ texOffset + Vector2 0.0 0.0
         , texOffset + Vector2 0.0 0.5
         , texOffset + Vector2 0.5 0.0
         , texOffset + Vector2 0.5 0.0
         , texOffset + Vector2 0.0 0.5
         , texOffset + Vector2 0.5 0.5
         ]

       normals = mesh'normals mesh ++ replicate 6 normal

   in mesh
      { mesh'vertices      = vertexData
      , mesh'normals       = normals
      , mesh'texcoords     = texCoords
      , mesh'triangleCount = mesh'triangleCount mesh + 2
      , mesh'vertexCount   = mesh'vertexCount mesh + 6
      }