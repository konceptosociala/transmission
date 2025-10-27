{-# LANGUAGE PatternSynonyms #-}
module LevelEditor where
   
import Raylib.Types (Camera3D(..), pattern Vector3, CameraProjection (CameraPerspective))

newtype SceneLevelEditor = SceneLevelEditor
   -- { leCurrentLevel :: Level
   { leCam :: Camera3D
   }

mkSceneLevelEditor :: SceneLevelEditor
mkSceneLevelEditor = SceneLevelEditor
   { leCam = Camera3D
      { camera3D'position = Vector3 5 5 5
      , camera3D'target = Vector3 0 0 0
      , camera3D'up = Vector3 0 1 0
      , camera3D'fovy = 70
      , camera3D'projection = CameraPerspective
      }
   }

updateLevelEditor :: SceneLevelEditor -> IO SceneLevelEditor
updateLevelEditor = pure