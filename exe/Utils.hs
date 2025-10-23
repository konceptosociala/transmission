module Utils where

import Paths_transmission (getDataFileName)
import Raylib.Util (managed, WindowResources)
import Raylib.Types (Model (model'materials), MaterialMapIndex (MaterialMapAlbedo))
import Raylib.Core.Models (loadModel, setMaterialTexture, loadMaterialDefault)
import Raylib.Core.Text (drawText, measureText)
import Control.Exception (try, IOException)
import Raylib.Core.Textures (loadImage, loadTextureFromImage)
import Raylib.Util.Colors

tryReadFile :: String -> IO (Maybe String)
tryReadFile file = do
   result <- try $ readFile file :: IO (Either IOException String)
   return $ case result of
      Left _  -> Nothing
      Right v -> Just v

loadTexturedModel :: WindowResources -> FilePath -> FilePath -> IO Model
loadTexturedModel w mdlP txtP = do
   mdl  <- managed w $ loadModel =<< getDataFileName mdlP
   img  <- loadImage =<< getDataFileName txtP
   txt  <- managed w $ loadTextureFromImage img
   mat  <- managed w loadMaterialDefault
   mat2 <- setMaterialTexture mat MaterialMapAlbedo txt

   return mdl { model'materials = [mat2] }

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

todo' :: String -> a
todo' err = error $ "TODO: " ++ err