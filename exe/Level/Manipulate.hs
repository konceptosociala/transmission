{-# LANGUAGE RecordWildCards #-}
module Level.Manipulate where

import Control.Monad.ST

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString as BL
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Bit
import Data.Void (Void)
import Data.Word (Word16)

import Level
import Level.Binary.BitParser
import Level.Binary

import Text.Megaparsec

dimsContain :: Dims -> (Word16, Word16, Word16) -> Bool
dimsContain (Dims w h d) (x, y, z) =
   not (x >= w || y >= h || z >= d)

index3 :: Dims -> (Word16, Word16, Word16) -> Int
index3 (Dims w h _) (x, y, z) = f x + (f y * f w) + (f z * f w * f h)
   where f = fromIntegral

newLevel :: MU.PrimMonad m 
   => Dims 
   -> m (MLevel (MU.PrimState m))
newLevel mlvlDims = do
   mlvlBlocks <- MU.replicate (dimsLen mlvlDims) (blockToFixed BEmpty)
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
      Just . fixedToBlock <$> MU.unsafeRead mv (index3 dims coords)

getBlockSolid :: MU.PrimMonad m
   => MLevel (MU.PrimState m)
   -> (Word16, Word16, Word16)
   -> m (Maybe Block)
getBlockSolid lvl coords = do
   mb <- getBlock lvl coords
   return $ case mb of
      Just b | b /= BEmpty -> Just b
      _ -> Nothing

hasBlock :: MU.PrimMonad m
   => MLevel (MU.PrimState m)
   -> (Word16, Word16, Word16)
   -> m Bool
hasBlock lvl coords = do
   mb <- getBlockSolid lvl coords
   return $ isJust mb

blockToType :: Block -> Maybe BlockType
blockToType BEmpty         = Nothing
blockToType BSolid         = Just BTSolid
blockToType (BHeight _ _)  = Just BTHeight
blockToType BPunch         = Just BTPunch
blockToType BFinish        = Just BTFinish

blockTypeCoords :: BlockType -> (Float, Float)
blockTypeCoords BTSolid   = (0, 0)
blockTypeCoords BTHeight  = (0.5, 0)
blockTypeCoords BTPunch   = (0, 0.5)
blockTypeCoords BTFinish  = (0.5, 0.5)

unfreezeLevel :: Level -> IO (MLevel MU.RealWorld)
unfreezeLevel (Level dims blocks) = do
   mv <- U.thaw blocks
   return $ MLevel dims mv

freezeLevel :: MU.PrimMonad m
   => MLevel (MU.PrimState m)
   -> m Level
freezeLevel (MLevel dims mv) =
   Level dims <$> U.unsafeFreeze mv

serializeLevel :: Level -> BS.ByteString
serializeLevel (Level (Dims w h d) blocks) =
   let hdr     = BB.word64BE magicNumber
       dimsB   = BB.word16BE w <> BB.word16BE h <> BB.word16BE d

       allBits = encodeBlocks $ map fixedToBlock $ U.toList blocks
       levelR  = cloneToByteString allBits
       level   = BB.byteString $ BS.map rev8 levelR

       nBits   = U.length allBits
       trBits  = (8 - (nBits `mod` 8)) `mod` 8
       pad     = BB.word8 $ fromIntegral trBits
   in BL.toStrict $ BB.toLazyByteString $
         hdr      -- 8 bytes MAGIC NUMBER
      <> dimsB    -- 6 bytes DIMS in U16
      <> pad      -- 1 byte 0-7 trailing bits
      <> level    -- 1D array of level data

deserializeMLevel :: MU.PrimMonad m
   => BS.ByteString 
   -> m (Either 
         (ParseErrorBundle BitStream Void) 
         (MLevel (MU.PrimState m))
      )
deserializeMLevel = runBitParserT levelP . bsToBitStream

deserializeLevel :: BS.ByteString -> Either (ParseErrorBundle BitStream Void) Level
deserializeLevel bs = runST $ do
   e <- deserializeMLevel bs
   case e of
      Left err   -> Left err & pure
      Right mlev -> Right <$> freezeLevel mlev
