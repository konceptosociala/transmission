module Scene.Game where

import Control.Monad (foldM)
import qualified Data.HashMap.Strict as HM

import Raylib.Types
import Raylib.Core.Models (uploadMesh)

import Level.Mesh (getAllChunkCoords, generateChunkMesh')
import Level (Level (lvlDims))

import Scene.MainMenu (mainMenuCam)

data SceneGame = SceneGame
   { gmLevel :: Level
   , gmChunkMeshes :: HM.HashMap (Int, Int, Int) Mesh
   , gmIsPaused :: Bool
   , gmIsOnline :: Bool
   , gmPlayer :: Player
   , gmPlayerCam :: Camera3D
   , gmPlayerNickname :: String
   , gmOtherPlayers :: [(String, Vector3)]
   , gmPlayersWon :: [String]
   }

mkSceneGame :: Level -> IO SceneGame
mkSceneGame lvl = do
   let dims = lvlDims lvl
   let chunkCoords = getAllChunkCoords dims
   
   chunks <- foldM genMesh HM.empty chunkCoords  
   return $ SceneGame
      { gmLevel = lvl
      , gmChunkMeshes = chunks
      , gmIsPaused = False
      , gmIsOnline = False
      , gmPlayer = Player
         { plPos = Vector3 0 5 0
         , plVel = Vector3 0 0 0
         , plOnGround = False
         }
      , gmPlayerCam = mainMenuCam
      , gmPlayerNickname = "Player"
      , gmOtherPlayers = []
      , gmPlayersWon = []
      }

   where
      genMesh hm coord = do
         m_ <- generateChunkMesh' lvl coord
         mesh <- uploadMesh m_ False
         return $ HM.insert coord mesh hm

data Player = Player
   { plPos :: Vector3
   , plVel :: Vector3
   , plOnGround :: Bool
   }