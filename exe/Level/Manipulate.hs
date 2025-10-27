{-# LANGUAGE RecordWildCards #-}
module Level.Manipulate where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word16)
import Level
import Level.Binary

dimsContain :: Dims -> (Word16, Word16, Word16) -> Bool
dimsContain (Dims w h d) (x, y, z) =
   not (x >= w || y >= h || z >= d)

index3 :: Dims -> (Word16, Word16, Word16) -> Int
index3 (Dims w h _) (x, y, z) = f x + (f y * f w) + (f z * f w * f h)
   where f = fromIntegral

dimsLen :: Dims -> Int
dimsLen (Dims w h d) = f w * f h * f d
   where f = fromIntegral

newLevel :: MU.PrimMonad m 
   => Dims 
   -> m (MLevel (MU.PrimState m))
newLevel mlvlDims = do
   mlvlBlocks <- MU.replicate (dimsLen mlvlDims) 0
   return MLevel {..}

setBlock :: MU.PrimMonad m
   => MLevel (MU.PrimState m)
   -> (Word16, Word16, Word16)
   -> Block
   -> m ()
setBlock (MLevel dims mv) coords block
   | not (dimsContain dims coords) = pure ()
   | otherwise =
      MU.unsafeWrite mv (index3 dims coords) (blockToFixed block)

getBlock :: MU.PrimMonad m
   => MLevel (MU.PrimState m)
   -> (Word16, Word16, Word16)
   -> m (Maybe Block)
getBlock (MLevel dims mv) coords
   | not (dimsContain dims coords) = pure Nothing
   | otherwise =
      Just . fixedToBlock <$> MU.unsafeRead mv 0

freezeLevel :: MU.PrimMonad m
   => MLevel (MU.PrimState m)
   -> m Level
freezeLevel (MLevel dims mv) =
   Level dims <$> U.unsafeFreeze mv