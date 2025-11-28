{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module Level.Binary where

import Control.Monad (forM_)
import Control.Monad.ST
import Control.Monad.Trans.Class (lift)

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Bit
import Data.Binary (Word8, Word64)
import Data.Bits (Bits(testBit, shiftL, shiftR, (.|.), (.&.)))

import Level
import Level.Binary.BitParser
import Level.Binary.BitMacro (bits)

import Text.Megaparsec

magicNumber :: Word64
magicNumber = 0xFA91D_AB0BA_09EC7_0 -- FARID, ABOBA, OPECT: big-endian

levelP :: MU.PrimMonad m => Parser m (MLevel (MU.PrimState m))
levelP = do
   _        <- magicNumberP
   mlvlDims <- dimsP <?> "dimensions"
   pad      <- word8 <?> "pad"
   skipLastBits (fromIntegral pad)

   let n = dimsLen mlvlDims
   mlvlBlocks <- do
      mv <- lift $ MU.unsafeNew n
      forM_ [0..n-1] $ \i -> do
         b <- blockToFixed <$> blockP
         lift $ MU.unsafeWrite mv i b
      pure mv

   return MLevel {..}

blockP :: Parser m Block
blockP = (BEmpty    <$ bitsExact [bits|111|])
     <-> (BSolid    <$ bitsExact [bits|110|])
     <-> (BHeight   <$> dirP <*> heightP)
     <-> (BPunch    <$ bitsExact [bits|000000|])
     <-> (BFinish   <$ bitsExact [bits|000001|])

dirP :: Parser m Direction
dirP = (DirUp     <$ bitsExact [bits|000|])
   <-> (DirDown   <$ bitsExact [bits|001|])
   <-> (DirFront  <$ bitsExact [bits|010|])
   <-> (DirBack   <$ bitsExact [bits|011|])
   <-> (DirRight  <$ bitsExact [bits|100|])
   <-> (DirLeft   <$ bitsExact [bits|101|])

heightP :: Parser m Word8
heightP = (1 <$ bitsExact [bits|010|])
      <-> (2 <$ bitsExact [bits|011|])
      <-> (3 <$ bitsExact [bits|100|])
      <-> (4 <$ bitsExact [bits|101|])
      <-> (5 <$ bitsExact [bits|110|])
      <-> (6 <$ bitsExact [bits|111|])

dimsP :: Parser m Dims
dimsP = Dims <$> word16 <*> word16 <*> word16

magicNumberP :: Parser m Word64
magicNumberP = word64Exact magicNumber (ErrorDescr "magic number" ReprHex)

encodeBlocks :: [Block] -> U.Vector Bit
encodeBlocks blocks = runST $ do
   let totalBits = sum $ map blockBitLen blocks
   mv <- MU.replicate totalBits (Bit False)
   go mv blocks 0

   U.unsafeFreeze mv
      where
         go :: MU.MVector s Bit -> [Block] -> Int -> ST s ()
         go _ [] _         = pure ()
         go mv (b:rest) i  = writeBlock mv i b >>= go mv rest

blockToFixed :: Block -> Word8
blockToFixed BEmpty           = 0b11111111
blockToFixed BSolid           = 0b11111110
blockToFixed BPunch           = 0b00000000
blockToFixed BFinish          = 0b00000001
blockToFixed (BHeight dir h)  = heightToBin dir h

fixedToBlock :: Word8 -> Block
fixedToBlock w8
   | w8 == 0b11111111 = BEmpty
   | w8 == 0b11111110 = BSolid
   | w8 == 0b00000000 = BPunch
   | w8 == 0b00000001 = BFinish
   | otherwise =
      let dirCode = (w8 `shiftR` 3) .&. 0b00000111
          heightVal = (w8 .&. 0b00000111) - 1
          dir = case dirCode of
            0b000 -> DirUp
            0b001 -> DirDown
            0b010 -> DirFront
            0b011 -> DirBack
            0b100 -> DirLeft
            0b101 -> DirRight
            e     -> error ("Invalid direction: " ++ show e)
      in BHeight dir (fromIntegral heightVal)

dirBits :: Direction -> Word8
dirBits DirUp    = 0b000
dirBits DirDown  = 0b001
dirBits DirFront = 0b010
dirBits DirBack  = 0b011
dirBits DirLeft  = 0b100
dirBits DirRight = 0b101

blockBitLen :: Block -> Int
blockBitLen BEmpty         = 3
blockBitLen BSolid         = 3
blockBitLen (BHeight _ _)  = 6
blockBitLen BPunch         = 6
blockBitLen BFinish        = 6

writeBlock :: MU.MVector s Bit -> Int -> Block -> ST s Int
writeBlock mv i b =
   case b of
      BEmpty         -> writeNBits mv i 3 0b111
      BSolid         -> writeNBits mv i 3 0b110
      BHeight dir h  -> writeNBits mv i 6 $ heightToBin dir h
      BPunch         -> writeNBits mv i 6 0b000
      BFinish        -> writeNBits mv i 6 0b001

heightToBin :: Direction -> Word8 -> Word8
heightToBin dir h
   | h < 1     = heightToBin dir 1
   | h > 6     = heightToBin dir 6
   | otherwise =
      let dirCode = dirBits dir
          val = fromIntegral (h + 1) :: Word8

      in (dirCode `shiftL` 3) .|. (val .&. 0b00000111)

writeNBits :: MU.MVector s Bit -> Int -> Int -> Word8 -> ST s Int
writeNBits mv i n byte = go 0 i
   where
      go k j
         | k == n    = pure j
         | otherwise = do
            let bitIdx = n - k - 1
            let b = testBit byte bitIdx

            MU.write mv j (Bit b)

            go (k + 1) (j + 1)

dimsLen :: Dims -> Int
dimsLen (Dims w h d) = f w * f h * f d
   where f = fromIntegral