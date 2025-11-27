{-# LANGUAGE PatternSynonyms #-}
module LevelEditor where
   
import Raylib.Types (Camera3D(..), pattern Vector3, CameraProjection (CameraPerspective), Matrix)
import Raylib.Util.Math (matrixTranslate)
import Control.Monad.ST (RealWorld)
import Level.Manipulate (deserializeMLevel, freezeLevel)
import Utils (eitherToMaybe)
import qualified Data.ByteString as BS
import Level.Mesh (generateMesh)
import Level
import Raylib.Types.Core.Models
import Debug.Trace (traceM)
import Raylib.Core.Models (uploadMesh)

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
   , leCurrentMesh :: Mesh
   , leSelectedBlock :: Maybe (Int, Int, Int)
   , leCurrentBlockType :: BlockType
   , leMeshMatrix :: Matrix
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
   m_ <- generateMesh lvl
   mesh <- uploadMesh m_ False
   traceM (show mesh)
   let Dims w _ d = mlvlDims lvl

   let offsetX = negate (fromIntegral (w `div` 2))
   let offsetZ = negate (fromIntegral (d `div` 2))
   let meshMatrix = matrixTranslate offsetX 0.0 offsetZ
   return $ SceneLevelEditor leCamDefault lvl descr mesh Nothing BTSolid meshMatrix

loadMLevel :: LevelDescr -> IO (Maybe (MLevel RealWorld))
loadMLevel (LevelDescr filename) = do
   loaded <- BS.readFile ("levels/"++filename)
   level  <- deserializeMLevel loaded
   return $ eitherToMaybe level

loadLevel :: LevelDescr -> IO (Maybe Level)
loadLevel d = traverse freezeLevel =<< loadMLevel d