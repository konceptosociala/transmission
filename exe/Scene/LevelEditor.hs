module Scene.LevelEditor where
   
import Control.Monad.ST (RealWorld)
import Control.Monad (foldM)

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath

import Raylib.Types
import Raylib.Core.Models (uploadMesh)

import Level.Manipulate (deserializeMLevel, freezeLevel)
import Level.Mesh (generateChunkMesh, getAllChunkCoords)
import Level

import Utils (eitherToMaybe)

newtype LevelDescr = LevelDescr FilePath

data SceneLevelEditorSelect = SceneLevelEditorSelect [LevelDescr] Int

data SceneNewLevel = SceneNewLevel
   { nlLabel :: String
   , nlDims :: Dims
   , nlSelectedItem :: SNLItem
   }

data SNLItem
   = SNLEditLabel
   | SNLEditDimW
   | SNLEditDimH
   | SNLEditDimD
   | SNLSubmit
   deriving Eq

nextSNLItem :: SNLItem -> SNLItem
nextSNLItem SNLEditLabel = SNLEditDimW
nextSNLItem SNLEditDimW = SNLEditDimH
nextSNLItem SNLEditDimH = SNLEditDimD
nextSNLItem SNLEditDimD = SNLSubmit
nextSNLItem SNLSubmit = SNLEditLabel

prevSNLItem :: SNLItem -> SNLItem
prevSNLItem SNLEditLabel = SNLSubmit
prevSNLItem SNLEditDimW = SNLEditLabel
prevSNLItem SNLEditDimH = SNLEditDimW
prevSNLItem SNLEditDimD = SNLEditDimH
prevSNLItem SNLSubmit = SNLEditDimD

data SceneLevelEditor = SceneLevelEditor
   { leCam :: Camera3D
   , leCurrentLevel :: MLevel RealWorld
   , leLevelDescr :: LevelDescr
   , leChunkMeshes :: HM.HashMap (Int, Int, Int) Mesh
   , leSelectedBlock :: Maybe (Int, Int, Int)
   , leCurrentBlockType :: BlockType
   }

leCamDefault :: Camera3D
leCamDefault = Camera3D
   { camera3D'position = Vector3 5 5 5
   , camera3D'target = Vector3 0 0 0
   , camera3D'up = Vector3 0 1 0
   , camera3D'fovy = 70
   , camera3D'projection = CameraPerspective
   }

mkSceneLevelEditor :: MLevel RealWorld -> LevelDescr -> IO SceneLevelEditor
mkSceneLevelEditor lvl descr = do
   let dims = mlvlDims lvl
   let chunkCoords = getAllChunkCoords dims
   
   chunks <- foldM genMesh HM.empty chunkCoords   
   return $ SceneLevelEditor leCamDefault lvl descr chunks Nothing BTSolid

   where
      genMesh hm coord = do
         m_ <- generateChunkMesh lvl coord
         mesh <- uploadMesh m_ False
         return $ HM.insert coord mesh hm

loadMLevel :: LevelDescr -> IO (Maybe (MLevel RealWorld))
loadMLevel (LevelDescr filename) = do
   loaded <- BS.readFile ("levels/"++filename)
   level  <- deserializeMLevel loaded
   return $ eitherToMaybe level

loadLevel :: LevelDescr -> IO (Maybe Level)
loadLevel d = traverse freezeLevel =<< loadMLevel d

loadLevels :: IO [LevelDescr]
loadLevels = do
   createDirectoryIfMissing False "levels"
   files <- getDirectoryContents "levels"
   let lvlFiles = filter (\f -> takeExtension f == ".lvl") files
   return $ map LevelDescr lvlFiles