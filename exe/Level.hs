{-# LANGUAGE PatternSynonyms #-}
module Level where

import GHC.Arr (Array)
import Raylib.Types (pattern Vector3, pattern Vector2, Mesh(..))
import Data.Function ((&))
import Raylib.Core.Models (uploadMesh)

newtype Level = Level (Array (Int, Int, Int) Block)

data Block
   = Empty
   | Solid
   | Height Direction Int
   | Finish

data Direction
   = DirUp
   | DirDown
   | DirFront
   | DirBack
   | DirRight
   | DirLeft

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
cube = do
   meshDefault
         & addFaceTop (0, 0, 0)
         & addFaceBottom (0, 0, 0)
         & addFaceFront (0, 0, 0)
         & addFaceBack (0, 0, 0)
         & addFaceLeft (0, 0, 0)
         & addFaceRight (0, 0, 0)
         & flip uploadMesh False

addFaceTop :: (Int, Int, Int) -> Mesh -> Mesh
addFaceTop (xi, yi, zi) mesh =
   let normal = Vector3 0 1 0
       (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)

       vertexData = mesh'vertices mesh ++
         [ Vector3 x (y + 1.0) z
         , Vector3 x (y + 1.0) (z + 1.0)
         , Vector3 (x + 1.0) (y + 1.0) z
         , Vector3 (x + 1.0) (y + 1.0) z
         , Vector3 x (y + 1.0) (z + 1.0)
         , Vector3 (x + 1.0) (y + 1.0) (z + 1.0)
         ]

       texCoords = mesh'texcoords mesh ++
         [ Vector2 0.0 0.0
         , Vector2 0.0 1.0
         , Vector2 1.0 0.0
         , Vector2 1.0 0.0
         , Vector2 0.0 1.0
         , Vector2 1.0 1.0
         ]

       normals = mesh'normals mesh ++ replicate 6 normal

   in mesh
      { mesh'vertices      = vertexData
      , mesh'normals       = normals
      , mesh'texcoords     = texCoords
      , mesh'triangleCount = mesh'triangleCount mesh + 2
      , mesh'vertexCount   = mesh'vertexCount mesh + 6
      }

addFaceBottom :: (Int, Int, Int) -> Mesh -> Mesh
addFaceBottom (xi, yi, zi) mesh =
   let normal = Vector3 0 (-1) 0
       (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)

       vertexData = mesh'vertices mesh ++
         [ Vector3 x y z
         , Vector3 (x + 1.0) y z
         , Vector3 x y (z + 1.0)
         , Vector3 (x + 1.0) y z
         , Vector3 (x + 1.0) y (z + 1.0)
         , Vector3 x y (z + 1.0)
         ]

       texCoords = mesh'texcoords mesh ++
         [ Vector2 0.0 1.0
         , Vector2 1.0 1.0
         , Vector2 0.0 0.0
         , Vector2 1.0 1.0
         , Vector2 1.0 0.0
         , Vector2 0.0 0.0
         ]

       normals = mesh'normals mesh ++ replicate 6 normal

   in mesh
      { mesh'vertices      = vertexData
      , mesh'normals       = normals
      , mesh'texcoords     = texCoords
      , mesh'triangleCount = mesh'triangleCount mesh + 2
      , mesh'vertexCount   = mesh'vertexCount mesh + 6
      }

addFaceFront :: (Int, Int, Int) -> Mesh -> Mesh
addFaceFront (xi, yi, zi) mesh =
   let normal = Vector3 0 0 1
       (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)

       vertexData = mesh'vertices mesh ++
         [ Vector3 x y z
         , Vector3 x (y + 1.0) z
         , Vector3 (x + 1.0) y z
         , Vector3 (x + 1.0) y z
         , Vector3 x (y + 1.0) z
         , Vector3 (x + 1.0) (y + 1.0) z
         ]

       texCoords = mesh'texcoords mesh ++
         [ Vector2 0.0 0.0
         , Vector2 0.0 1.0
         , Vector2 1.0 0.0
         , Vector2 1.0 0.0
         , Vector2 0.0 1.0
         , Vector2 1.0 1.0
         ]

       normals = mesh'normals mesh ++ replicate 6 normal

   in mesh
      { mesh'vertices      = vertexData
      , mesh'normals       = normals
      , mesh'texcoords     = texCoords
      , mesh'triangleCount = mesh'triangleCount mesh + 2
      , mesh'vertexCount   = mesh'vertexCount mesh + 6
      }

addFaceBack :: (Int, Int, Int) -> Mesh -> Mesh
addFaceBack (xi, yi, zi) mesh = 
   let normal = Vector3 0 0 (-1)
       (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)

       vertexData = mesh'vertices mesh ++
         [ Vector3 x y (z + 1.0)
         , Vector3 (x + 1.0) y (z + 1.0)
         , Vector3 x (y + 1.0) (z + 1.0)
         , Vector3 (x + 1.0) y (z + 1.0)
         , Vector3 (x + 1.0) (y + 1.0) (z + 1.0)
         , Vector3 x (y + 1.0) (z + 1.0)
         ]

       texCoords = mesh'texcoords mesh ++
         [ Vector2 1.0 0.0
         , Vector2 0.0 0.0
         , Vector2 1.0 1.0
         , Vector2 0.0 0.0
         , Vector2 0.0 1.0
         , Vector2 1.0 1.0
         ]

       normals = mesh'normals mesh ++ replicate 6 normal

   in mesh
      { mesh'vertices      = vertexData
      , mesh'normals       = normals
      , mesh'texcoords     = texCoords
      , mesh'triangleCount = mesh'triangleCount mesh + 2
      , mesh'vertexCount   = mesh'vertexCount mesh + 6
      }

addFaceLeft :: (Int, Int, Int) -> Mesh -> Mesh
addFaceLeft (xi, yi, zi) mesh =
   let normal = Vector3 (-1) 0 0
       (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)

       vertexData = mesh'vertices mesh ++
         [ Vector3 x y z
         , Vector3 x y (z + 1.0)
         , Vector3 x (y + 1.0) z
         , Vector3 x (y + 1.0) z
         , Vector3 x y (z + 1.0)
         , Vector3 x (y + 1.0) (z + 1.0)
         ]

       texCoords = mesh'texcoords mesh ++
         [ Vector2 1.0 0.0
         , Vector2 0.0 0.0
         , Vector2 1.0 1.0
         , Vector2 1.0 1.0
         , Vector2 0.0 0.0
         , Vector2 0.0 1.0
         ]

       normals = mesh'normals mesh ++ replicate 6 normal

   in mesh
      { mesh'vertices      = vertexData
      , mesh'normals       = normals
      , mesh'texcoords     = texCoords
      , mesh'triangleCount = mesh'triangleCount mesh + 2
      , mesh'vertexCount   = mesh'vertexCount mesh + 6
      }

addFaceRight :: (Int, Int, Int) -> Mesh -> Mesh
addFaceRight (xi, yi, zi) mesh =
   let normal = Vector3 1 0 0
       (x, y, z) = (fromIntegral xi, fromIntegral yi, fromIntegral zi)

       vertexData = mesh'vertices mesh ++
         [ Vector3 (x + 1.0) y z
         , Vector3 (x + 1.0) (y + 1.0) z
         , Vector3 (x + 1.0) y (z + 1.0)
         , Vector3 (x + 1.0) y (z + 1.0)
         , Vector3 (x + 1.0) (y + 1.0) z
         , Vector3 (x + 1.0) (y + 1.0) (z + 1.0)
         ]

       texCoords = mesh'texcoords mesh ++
         [ Vector2 0.0 0.0
         , Vector2 0.0 1.0
         , Vector2 1.0 0.0
         , Vector2 1.0 0.0
         , Vector2 0.0 1.0
         , Vector2 1.0 1.0
         ]

       normals = mesh'normals mesh ++ replicate 6 normal

   in mesh
      { mesh'vertices      = vertexData
      , mesh'normals       = normals
      , mesh'texcoords     = texCoords
      , mesh'triangleCount = mesh'triangleCount mesh + 2
      , mesh'vertexCount   = mesh'vertexCount mesh + 6
      }