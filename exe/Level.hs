module Level where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8, Word16)
import Data.Bits (Bits(testBit))

data Dims = Dims { dimsW :: !Word16, dimsH :: !Word16, dimsD :: !Word16 }

data Level = Level
   { lvlDims :: !Dims
   , lvlBlocks :: !(U.Vector Word8)
   }

instance Show Dims where
   show (Dims w h d) = "Dims:{"++show w++"x"++show h++"x"++show d++"}"

instance Show Level where
   show (Level dims blocks) = 
      "Level:\n"
         ++show dims++"\n"
         ++show (map blockW8toStr (U.toList blocks))

data MLevel s = MLevel
   { mlvlDims :: !Dims
   , mlvlBlocks :: !(MU.MVector s Word8)
   }

data Block
   = BEmpty                   -- Transparent
   | BSolid                   -- White
   | BHeight Direction Word8  -- Yellow
   | BPunch                   -- Blue
   | BFinish                  -- Black
   deriving (Eq, Show)

data Direction
   = DirUp
   | DirDown
   | DirFront
   | DirBack
   | DirRight
   | DirLeft
   deriving (Eq, Show)

blockW8toStr :: Word8 -> String
blockW8toStr w8
   | w8 == 0b00000000 = "000"
   | w8 == 0b00000001 = "001"
   | w8 == 0b11111110 = "110"
   | w8 == 0b11111111 = "111"
   | otherwise = drop 2 $ concatMap (show . fromEnum) bits
      where
         bits = [ testBit w8 i | i <- [7,6..0] ]