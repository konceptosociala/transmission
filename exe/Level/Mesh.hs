{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Level.Mesh where

import Raylib.Types
import Level
import Level.Manipulate
import Control.Monad (forM_, foldM)
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Unboxed as U
import Utils
import Data.Binary (Word16)

cube :: IO Mesh
cube = do
   mb0 <- newMeshBuilder 6
   mb1 <- addFaceTop      (0 :: Int, 0, 0) BTSolid mb0
   mb2 <- addFaceBottom   (0 :: Int, 0, 0) BTSolid mb1
   mb3 <- addFaceFront    (0 :: Int, 0, 0) BTSolid mb2
   mb4 <- addFaceBack     (0 :: Int, 0, 0) BTSolid mb3
   mb5 <- addFaceLeft     (0 :: Int, 0, 0) BTSolid mb4
   mb6 <- addFaceRight    (0 :: Int, 0, 0) BTSolid mb5
   freezeMesh mb6

data MeshBuilder s = MeshBuilder
   { mbVertices      :: MU.MVector s Float
   , mbTexcoords     :: MU.MVector s Float
   , mbNormals       :: MU.MVector s Float
   , mbVertexOffset  :: Int
   , mbTexOffset     :: Int
   , mbNormOffset    :: Int
   , mbTriangleCount :: Int
   }

newMeshBuilder :: MU.PrimMonad m
   => Int
   -> m (MeshBuilder (MU.PrimState m))
newMeshBuilder maxFaces = do
   mv <- MU.new (maxFaces * 6 * 3)
   mt <- MU.new (maxFaces * 6 * 2)
   mn <- MU.new (maxFaces * 6 * 3)

   return MeshBuilder
      { mbVertices      = mv
      , mbTexcoords     = mt
      , mbNormals       = mn
      , mbVertexOffset  = 0
      , mbTexOffset     = 0
      , mbNormOffset    = 0
      , mbTriangleCount = 0
      }

freezeMesh :: MU.PrimMonad m
   => MeshBuilder (MU.PrimState m)
   -> m Mesh
freezeMesh MeshBuilder {..} = do
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

generateMesh :: MLevel MU.RealWorld -> IO Mesh
generateMesh lvl = do
   let Dims w h d = mlvlDims lvl
   let f = fromIntegral
   let maxFaces = f w * f h * f d * 6

   mb <- newMeshBuilder maxFaces   

   mbFinal <- foldM (processBlock lvl w h d) mb
      [ (x, y, z)
      | x <- [0 .. w - 1]
      , y <- [0 .. h - 1]
      , z <- [0 .. d - 1]
      ]

   freezeMesh mbFinal

generateChunkMesh :: MLevel MU.RealWorld -> Word16 -> (Int, Int, Int) -> IO Mesh
generateChunkMesh lvl chunkSize (cx, cy, cz) = do
   let Dims w h d = mlvlDims lvl
   
   let chunkX0 = fromIntegral cx * fromIntegral chunkSize
   let chunkY0 = fromIntegral cy * fromIntegral chunkSize
   let chunkZ0 = fromIntegral cz * fromIntegral chunkSize
   
   let chunkX1 = min (chunkX0 + fromIntegral chunkSize) w
   let chunkY1 = min (chunkY0 + fromIntegral chunkSize) h
   let chunkZ1 = min (chunkZ0 + fromIntegral chunkSize) d
   
   let f = fromIntegral
   let chunkW = chunkX1 - chunkX0
   let chunkH = chunkY1 - chunkY0
   let chunkD = chunkZ1 - chunkZ0
   let maxFaces = f chunkW * f chunkH * f chunkD * 6
   
   mb <- newMeshBuilder maxFaces
   
   -- Process blocks with chunk-relative coordinates
   mbFinal <- foldM (processBlockChunk lvl w h d chunkX0 chunkY0 chunkZ0) mb
      [ (x, y, z)
      | x <- [chunkX0 .. chunkX1 - 1]
      , y <- [chunkY0 .. chunkY1 - 1]
      , z <- [chunkZ0 .. chunkZ1 - 1]
      ]
   
   freezeMesh mbFinal

-- Process block for chunk mesh (using relative coordinates)
processBlockChunk :: MU.PrimMonad m
  => MLevel (MU.PrimState m)
  -> Word16 -> Word16 -> Word16  -- Level dimensions
  -> Word16 -> Word16 -> Word16  -- Chunk offset
  -> MeshBuilder (MU.PrimState m)
  -> (Word16, Word16, Word16)    -- Absolute block coords
  -> m (MeshBuilder (MU.PrimState m))
processBlockChunk lvl w h d chunkX0 chunkY0 chunkZ0 mb (x, y, z) = do
   currentBlock <- getBlockSolid lvl (x, y, z)
   case blockToType =<< currentBlock of
      Nothing -> return mb
      Just ty -> do
         hasTop    <- if y == h - 1 then return False else hasBlock lvl (x, y+1, z)
         hasBottom <- if y == 0     then return False else hasBlock lvl (x, y-1, z)
         hasFront  <- if z == d - 1 then return False else hasBlock lvl (x, y, z+1)
         hasBack   <- if z == 0     then return False else hasBlock lvl (x, y, z-1)
         hasLeft   <- if x == 0     then return False else hasBlock lvl (x-1, y, z)
         hasRight  <- if x == w - 1 then return False else hasBlock lvl (x+1, y, z)

         -- Use chunk-relative coordinates for mesh generation
         let relX = x - chunkX0
         let relY = y - chunkY0
         let relZ = z - chunkZ0

         mb1 <- if not hasTop    then addFaceTop    (relX,relY,relZ) ty mb else return mb
         mb2 <- if not hasBottom then addFaceBottom (relX,relY,relZ) ty mb1 else return mb1
         mb3 <- if not hasFront  then addFaceFront  (relX,relY,relZ) ty mb2 else return mb2
         mb4 <- if not hasBack   then addFaceBack   (relX,relY,relZ) ty mb3 else return mb3
         mb5 <- if not hasLeft   then addFaceLeft   (relX,relY,relZ) ty mb4 else return mb4
         mb6 <- if not hasRight  then addFaceRight  (relX,relY,relZ) ty mb5 else return mb5
         return mb6

processBlock :: MU.PrimMonad m
  => MLevel (MU.PrimState m)
  -> Word16 -> Word16 -> Word16
  -> MeshBuilder (MU.PrimState m)
  -> (Word16, Word16, Word16)
  -> m (MeshBuilder (MU.PrimState m))
processBlock lvl w h d mb (x, y, z) = do
   currentBlock <- getBlockSolid lvl (x, y, z)
   case blockToType =<< currentBlock of
      Nothing -> return mb
      Just ty -> do
         hasTop    <- if y == h - 1 then return False else hasBlock lvl (x, y+1, z)
         hasBottom <- if y == 0     then return False else hasBlock lvl (x, y-1, z)
         hasFront  <- if z == d - 1 then return False else hasBlock lvl (x, y, z+1)
         hasBack   <- if z == 0     then return False else hasBlock lvl (x, y, z-1)
         hasLeft   <- if x == 0     then return False else hasBlock lvl (x-1, y, z)
         hasRight  <- if x == w - 1 then return False else hasBlock lvl (x+1, y, z)

         mb1 <- if not hasTop    then addFaceTop    (x,y,z) ty mb else return mb
         mb2 <- if not hasBottom then addFaceBottom (x,y,z) ty mb1 else return mb1
         mb3 <- if not hasFront  then addFaceFront  (x,y,z) ty mb2 else return mb2
         mb4 <- if not hasBack   then addFaceBack   (x,y,z) ty mb3 else return mb3
         mb5 <- if not hasLeft   then addFaceLeft   (x,y,z) ty mb4 else return mb4
         mb6 <- if not hasRight  then addFaceRight  (x,y,z) ty mb5 else return mb5

         return mb6

addFaceTop :: (MU.PrimMonad m, Integral a)
  => (a, a, a)
  -> Level.BlockType
  -> MeshBuilder (MU.PrimState m)
  -> m (MeshBuilder (MU.PrimState m))
addFaceTop (xi, yi, zi) ty mb =
   let (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)
       (u, v) = blockTypeCoords ty

       verts =
         [ (x,       y + 1, z)
         , (x,       y + 1, z + 1)
         , (x + 1,   y + 1, z)
         , (x + 1,   y + 1, z)
         , (x,       y + 1, z + 1)
         , (x + 1,   y + 1, z + 1)
         ]

       uvs =
         [ (u,       v)
         , (u,       v + 0.5)
         , (u + 0.5, v)
         , (u + 0.5, v)
         , (u,       v + 0.5)
         , (u + 0.5, v + 0.5)
         ]

       vo = mbVertexOffset mb
       to = mbTexOffset mb
       no = mbNormOffset mb

   in do
      -- Write vertices
      forM_ (zip [0..] verts) $ \(i, (vx,vy,vz)) -> do
         let j = vo + i * 3
         MU.write (mbVertices mb) j     vx
         MU.write (mbVertices mb) (j+1) vy
         MU.write (mbVertices mb) (j+2) vz

      -- Write texcoords
      forM_ (zip [0..] uvs) $ \(i, (tu,tv)) -> do
         let j = to + i * 2
         MU.write (mbTexcoords mb) j     tu
         MU.write (mbTexcoords mb) (j+1) tv

      -- Write normals
      forM_ [0..5] $ \i -> do
         let j = no + i * 3
         MU.write (mbNormals mb) j     0
         MU.write (mbNormals mb) (j+1) 1
         MU.write (mbNormals mb) (j+2) 0

      -- update offsets
      pure mb
         { mbVertexOffset  = vo + 18
         , mbTexOffset     = to + 12
         , mbNormOffset    = no + 18
         , mbTriangleCount = mbTriangleCount mb + 2
         }

addFaceBottom :: (MU.PrimMonad m, Integral a)
  => (a, a, a)
  -> Level.BlockType
  -> MeshBuilder (MU.PrimState m)
  -> m (MeshBuilder (MU.PrimState m))
addFaceBottom (xi, yi, zi) ty mb =
   let (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)
       (u, v) = blockTypeCoords ty

       verts =
         [ (x,       y, z)
         , (x + 1,   y, z)
         , (x,       y, z + 1)
         , (x + 1,   y, z)
         , (x + 1,   y, z + 1)
         , (x,       y, z + 1)
         ]

       uvs =
         [ (u,       v + 0.5)
         , (u + 0.5, v + 0.5)
         , (u,       v)
         , (u + 0.5, v + 0.5)
         , (u + 0.5, v)
         , (u,       v)
         ]

       vo = mbVertexOffset mb
       to = mbTexOffset mb
       no = mbNormOffset mb

   in do
      -- Write vertices
      forM_ (zip [0..] verts) $ \(i, (vx,vy,vz)) -> do
         let j = vo + i * 3
         MU.write (mbVertices mb) j     vx
         MU.write (mbVertices mb) (j+1) vy
         MU.write (mbVertices mb) (j+2) vz

      -- Write texcoords
      forM_ (zip [0..] uvs) $ \(i, (tu,tv)) -> do
         let j = to + i * 2
         MU.write (mbTexcoords mb) j     tu
         MU.write (mbTexcoords mb) (j+1) tv

      -- Write normals
      forM_ [0..5] $ \i -> do
         let j = no + i * 3
         MU.write (mbNormals mb) j     0
         MU.write (mbNormals mb) (j+1) (-1)
         MU.write (mbNormals mb) (j+2) 0

      -- update offsets
      pure mb
         { mbVertexOffset  = vo + 18
         , mbTexOffset     = to + 12
         , mbNormOffset    = no + 18
         , mbTriangleCount = mbTriangleCount mb + 2
         }

addFaceFront :: (MU.PrimMonad m, Integral a)
  => (a, a, a)
  -> Level.BlockType
  -> MeshBuilder (MU.PrimState m)
  -> m (MeshBuilder (MU.PrimState m))
addFaceFront (xi, yi, zi) ty mb =
   let (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)
       (u, v) = blockTypeCoords ty

       verts =
         [ (x,       y,       z + 1)
         , (x + 1,   y,       z + 1)
         , (x,       y + 1,   z + 1)
         , (x + 1,   y,       z + 1)
         , (x + 1,   y + 1,   z + 1)
         , (x,       y + 1,   z + 1)
         ]

       uvs =
         [ (u + 0.5, v)
         , (u,       v)
         , (u + 0.5, v + 0.5)
         , (u,       v)
         , (u,       v + 0.5)
         , (u + 0.5, v + 0.5)
         ]

       vo = mbVertexOffset mb
       to = mbTexOffset mb
       no = mbNormOffset mb

   in do
      -- Write vertices
      forM_ (zip [0..] verts) $ \(i, (vx,vy,vz)) -> do
         let j = vo + i * 3
         MU.write (mbVertices mb) j     vx
         MU.write (mbVertices mb) (j+1) vy
         MU.write (mbVertices mb) (j+2) vz

      -- Write texcoords
      forM_ (zip [0..] uvs) $ \(i, (tu,tv)) -> do
         let j = to + i * 2
         MU.write (mbTexcoords mb) j     tu
         MU.write (mbTexcoords mb) (j+1) tv

      -- Write normals
      forM_ [0..5] $ \i -> do
         let j = no + i * 3
         MU.write (mbNormals mb) j     0
         MU.write (mbNormals mb) (j+1) 0
         MU.write (mbNormals mb) (j+2) 1

      -- update offsets
      pure mb
         { mbVertexOffset  = vo + 18
         , mbTexOffset     = to + 12
         , mbNormOffset    = no + 18
         , mbTriangleCount = mbTriangleCount mb + 2
         }

addFaceBack :: (MU.PrimMonad m, Integral a)
  => (a, a, a)
  -> Level.BlockType
  -> MeshBuilder (MU.PrimState m)
  -> m (MeshBuilder (MU.PrimState m))
addFaceBack (xi, yi, zi) ty mb =
   let (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)
       (u, v) = blockTypeCoords ty

       verts =
         [ (x,       y,       z)
         , (x,       y + 1,   z)
         , (x + 1,   y,       z)
         , (x + 1,   y,       z)
         , (x,       y + 1,   z)
         , (x + 1,   y + 1,   z)
         ]

       uvs =
         [ (u,       v)
         , (u,       v + 0.5)
         , (u + 0.5, v)
         , (u + 0.5, v)
         , (u,       v + 0.5)
         , (u + 0.5, v + 0.5)
         ]

       vo = mbVertexOffset mb
       to = mbTexOffset mb
       no = mbNormOffset mb

   in do
      -- Write vertices
      forM_ (zip [0..] verts) $ \(i, (vx,vy,vz)) -> do
         let j = vo + i * 3
         MU.write (mbVertices mb) j     vx
         MU.write (mbVertices mb) (j+1) vy
         MU.write (mbVertices mb) (j+2) vz

      -- Write texcoords
      forM_ (zip [0..] uvs) $ \(i, (tu,tv)) -> do
         let j = to + i * 2
         MU.write (mbTexcoords mb) j     tu
         MU.write (mbTexcoords mb) (j+1) tv

      -- Write normals
      forM_ [0..5] $ \i -> do
         let j = no + i * 3
         MU.write (mbNormals mb) j     0
         MU.write (mbNormals mb) (j+1) 0
         MU.write (mbNormals mb) (j+2) (-1)

      -- update offsets
      pure mb
         { mbVertexOffset  = vo + 18
         , mbTexOffset     = to + 12
         , mbNormOffset    = no + 18
         , mbTriangleCount = mbTriangleCount mb + 2
         }

addFaceLeft :: (MU.PrimMonad m, Integral a)
   => (a, a, a)
   -> Level.BlockType
   -> MeshBuilder (MU.PrimState m)
   -> m (MeshBuilder (MU.PrimState m))
addFaceLeft (xi, yi, zi) ty mb =
   let (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)
       (u, v) = blockTypeCoords ty

       verts =
         [ (x, y,       z)
         , (x, y,       z + 1)
         , (x, y + 1,   z)
         , (x, y + 1,   z)
         , (x, y,       z + 1)
         , (x, y + 1,   z + 1)
         ]

       uvs =
            [ (u + 0.5, v)
            , (u,       v)
            , (u + 0.5, v + 0.5)
            , (u + 0.5, v + 0.5)
            , (u,       v)
            , (u,       v + 0.5)
            ]

       vo = mbVertexOffset mb
       to = mbTexOffset mb
       no = mbNormOffset mb

   in do
      -- Write vertices
      forM_ (zip [0..] verts) $ \(i, (vx,vy,vz)) -> do
         let j = vo + i * 3
         MU.write (mbVertices mb) j     vx
         MU.write (mbVertices mb) (j+1) vy
         MU.write (mbVertices mb) (j+2) vz

      -- Write texcoords
      forM_ (zip [0..] uvs) $ \(i, (tu,tv)) -> do
         let j = to + i * 2
         MU.write (mbTexcoords mb) j     tu
         MU.write (mbTexcoords mb) (j+1) tv

      -- Write normals
      forM_ [0..5] $ \i -> do
         let j = no + i * 3
         MU.write (mbNormals mb) j     (-1)
         MU.write (mbNormals mb) (j+1) 0
         MU.write (mbNormals mb) (j+2) 0

      -- update offsets
      pure mb
         { mbVertexOffset  = vo + 18
         , mbTexOffset     = to + 12
         , mbNormOffset    = no + 18
         , mbTriangleCount = mbTriangleCount mb + 2
         }

addFaceRight :: (MU.PrimMonad m, Integral a)
   => (a, a, a)
   -> Level.BlockType
   -> MeshBuilder (MU.PrimState m)
   -> m (MeshBuilder (MU.PrimState m))
addFaceRight (xi, yi, zi) ty mb =
   let (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)
       (u, v) = blockTypeCoords ty

       verts =
         [ (x + 1,   y,       z)
         , (x + 1,   y + 1,   z)
         , (x + 1,   y,       z + 1)
         , (x + 1,   y,       z + 1)
         , (x + 1,   y + 1,   z)
         , (x + 1,   y + 1,   z + 1)
         ]

       uvs =
         [ (u,       v)
         , (u,       v + 0.5)
         , (u + 0.5, v)
         , (u + 0.5, v)
         , (u,       v + 0.5)
         , (u + 0.5, v + 0.5)
         ]

       vo = mbVertexOffset mb
       to = mbTexOffset mb
       no = mbNormOffset mb

   in do
      -- Write vertices
      forM_ (zip [0..] verts) $ \(i, (vx,vy,vz)) -> do
         let j = vo + i * 3
         MU.write (mbVertices mb) j     vx
         MU.write (mbVertices mb) (j+1) vy
         MU.write (mbVertices mb) (j+2) vz

      -- Write texcoords
      forM_ (zip [0..] uvs) $ \(i, (tu,tv)) -> do
         let j = to + i * 2
         MU.write (mbTexcoords mb) j     tu
         MU.write (mbTexcoords mb) (j+1) tv

      -- Write normals
      forM_ [0..5] $ \i -> do
         let j = no + i * 3
         MU.write (mbNormals mb) j     1
         MU.write (mbNormals mb) (j+1) 0
         MU.write (mbNormals mb) (j+2) 0

      -- update offsets
      pure mb
         { mbVertexOffset  = vo + 18
         , mbTexOffset     = to + 12
         , mbNormOffset    = no + 18
         , mbTriangleCount = mbTriangleCount mb + 2
         }