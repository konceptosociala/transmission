module Level  where

import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8, Word16)

data Dims = Dims { dimsW :: !Word16, dimsH :: !Word16, dimsD :: !Word16 }

-- dimsLen :: Dims -> Int
-- dimsLen (Dims w h d) = f w * f h * f d
--    where f = fromIntegral

data Level = Level
   { lvlDims :: !Dims
   , lvlBlocks :: !(U.Vector Word8)
   }

data MLevel s = MLevel
   { mlvlDims :: !Dims
   , mlvlBlocks :: !(MU.MVector s Word8)
   }

idx :: Dims -> (Int, Int, Int) -> Int
idx (Dims w h _) (x, y, z) = x + y * f w + z * f w * f h
   where f = fromIntegral

data Block
   = BEmpty                -- Transparent
   | BSolid                -- White
   | BHeight Direction Int -- Yellow
   | BPunch                -- Blue
   | BFinish               -- Black

data Direction
   = DirUp
   | DirDown
   | DirFront
   | DirBack
   | DirRight
   | DirLeft