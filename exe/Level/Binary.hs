module Level.Binary where

import Data.Bit
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Binary (Word8, Word64)
import Data.Bits (Bits(testBit, shiftL, shiftR))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString as BL
import Foreign (Bits((.|.), (.&.)))
import Level

magicNumber :: Word64
magicNumber = 0xFA91D_AB0BA_09EC7_0 -- FARID, ABOBA, OPECT: big-endian

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

deserializeLevel :: BS.ByteString -> Level
deserializeLevel bs = undefined

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

rev8 :: Word8 -> Word8
rev8 x =
   let x1 = ((x .&. 0xF0) `shiftR` 4) .|. ((x .&. 0x0F) `shiftL` 4)
       x2 = ((x1 .&. 0xCC) `shiftR` 2) .|. ((x1 .&. 0x33) `shiftL` 2) 
   in       ((x2 .&. 0xAA) `shiftR` 1) .|. ((x2 .&. 0x55) `shiftL` 1)

blockToFixed :: Block -> Word8
blockToFixed BEmpty           = 0b00000000
blockToFixed BSolid           = 0b00000001
blockToFixed BPunch           = 0b11111110
blockToFixed BFinish          = 0b11111111
blockToFixed (BHeight dir h)  = heightToBin dir h

fixedToBlock :: Word8 -> Block
fixedToBlock w8
   | w8 == 0b00000000 = BEmpty
   | w8 == 0b00000001 = BSolid
   | w8 == 0b11111110 = BPunch
   | w8 == 0b11111111 = BFinish
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
blockBitLen BEmpty         = 6
blockBitLen BSolid         = 6
blockBitLen (BHeight _ _)  = 6
blockBitLen BPunch         = 3
blockBitLen BFinish        = 3

writeBlock :: MU.MVector s Bit -> Int -> Block -> ST s Int
writeBlock mv i b =
   case b of
      BEmpty         -> writeNBits mv i 6 0b000
      BSolid         -> writeNBits mv i 6 0b001
      BHeight dir h  -> writeNBits mv i 6 $ heightToBin dir h
      BPunch         -> writeNBits mv i 3 0b110
      BFinish        -> writeNBits mv i 3 0b111

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

