{-# LANGUAGE TypeFamilies #-}
module Level.Binary.BitParser
   ( runBitParserT
   , bit, bitExact, bits, bitsExact, bitsAs
   , word8, word8Exact
   , word16, word16Exact
   , word32, word32Exact
   , word64, word64Exact
   , skipLastBits
   , BitStream (..), ErrorRepr (..), ErrorDescr (..)
   , Parser
   , bitToStr, bitsToStr, bsToBitStream, rev8, (<->)
   ) where

import Data.Void (Void)
import Text.Megaparsec (Stream (..), ParseErrorBundle, anySingle, VisualStream(..), TraversableStream(..), PosState(..), updateParserState, State (stateInput), ParsecT, runParserT, observing, MonadParsec (try, parseError))
import qualified Data.Vector.Unboxed as U
import Data.Bit (Bit(..), cloneFromByteString)
import Data.Proxy (Proxy)
import Control.Monad (replicateM)
import Data.Binary (Word8, Word32, Word64)
import Data.Bits (Bits((.|.), shiftL, shiftR), (.&.))
import Data.Word (Word16)
import Text.Printf (printf)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE

data ErrorDescr = ErrorDescr
   { label :: String
   , repr  :: ErrorRepr
   }

data ErrorRepr
   = ReprBin
   | ReprHex
   | ReprDec
   deriving (Eq, Show)

newtype BitStream = BitStream (U.Vector Bit)

instance Show BitStream where
   show :: BitStream -> String
   show (BitStream v) = bitsToStr v

instance TraversableStream BitStream where
   reachOffset :: Int -> PosState BitStream -> (Maybe String, PosState BitStream)
   reachOffset o ps =
      let BitStream input = pstateInput ps
          offset = max 0 (min o (U.length input))
          rest   = BitStream (U.drop offset input)
          start  = max 0 (offset - 16)
          end    = min (U.length input) (offset + 16)
          snippet = bitsToStr (U.slice start (end - start) input)
          marker  = replicate (offset - start) ' ' ++ "^"
          line    = Just $ snippet ++ "\n" ++ marker
          ps' = ps
            { pstateInput = rest
            , pstateOffset = offset
            , pstateSourcePos = pstateSourcePos ps
            }
    in (line, ps')

   reachOffsetNoLine :: Int -> PosState BitStream -> PosState BitStream
   reachOffsetNoLine o ps =
      let BitStream input = pstateInput ps
          offset = max 0 (min o (U.length input))
          rest   = BitStream (U.drop offset input)
      in ps
         { pstateInput = rest
         , pstateOffset = offset
         , pstateSourcePos = pstateSourcePos ps
         }

instance VisualStream BitStream where
   showTokens :: Proxy BitStream -> NE.NonEmpty (Token BitStream) -> String
   showTokens _ = bitsToStr . U.fromList . NE.toList

   tokensLength :: Proxy BitStream -> NE.NonEmpty (Token BitStream) -> Int
   tokensLength _ = NE.length

instance Stream BitStream where
   type Token BitStream = Bit
   type Tokens BitStream = U.Vector Bit

   tokenToChunk :: Proxy BitStream -> Token BitStream -> Tokens BitStream
   tokenToChunk _ = U.singleton

   tokensToChunk :: Proxy BitStream -> [Token BitStream] -> Tokens BitStream
   tokensToChunk _ = U.fromList

   chunkToTokens :: Proxy BitStream -> Tokens BitStream -> [Token BitStream]
   chunkToTokens _ = U.toList

   chunkLength :: Proxy BitStream -> Tokens BitStream -> Int
   chunkLength _ = U.length

   chunkEmpty :: Proxy BitStream -> Tokens BitStream -> Bool
   chunkEmpty _ = U.null

   take1_ :: BitStream -> Maybe (Token BitStream, BitStream)
   take1_ (BitStream v)
      | U.null v  = Nothing
      | otherwise = Just (U.unsafeIndex v 0, BitStream (U.tail v))

   takeN_ :: Int -> BitStream -> Maybe (Tokens BitStream, BitStream)
   takeN_ n (BitStream v)
      | n <= 0          = Just (U.empty, BitStream v)
      | n > U.length v  = Nothing
      | otherwise       = let (a,b) = U.splitAt n v in Just (a, BitStream b)

   takeWhile_ :: (Token BitStream -> Bool) -> BitStream -> (Tokens BitStream, BitStream)
   takeWhile_ p (BitStream v) =
      let (pref, rest) = U.span p v
      in (pref, BitStream rest)

type Parser m = ParsecT Void BitStream m

runBitParserT :: Monad m => Parser m a -> BitStream -> m (Either (ParseErrorBundle BitStream Void) a)
runBitParserT p = runParserT p "<bits>"

bit :: Parser m Bit
bit = anySingle

bitExact :: Bit -> Parser m Bit
bitExact e = do
   f <- bit

   if e == f
      then return f
      else fail ("exptected `"++bitToStr e++"`, found `"++bitToStr f++"`")

bits :: Int -> Parser m (U.Vector Bit)
bits n
   | n <= 0    = return U.empty
   | otherwise = U.fromList <$> replicateM n anySingle

bitsExact :: U.Vector Bit -> Parser m (U.Vector Bit)
bitsExact es = do
   fs <- bits (U.length es)

   if es == fs
      then return fs
      else fail ("expected `"++bitsToStr es++"`, found `"++bitsToStr fs++"`")

bitsAs :: (Bits a, Num a) => Int -> Parser m a
bitsAs n
   | n <= 0    = return 0
   | otherwise = go 0 n
      where
         go :: (Bits a, Num a) => a -> Int -> Parser m a
         go acc 0 = return acc
         go acc k = do
            (Bit b) <- anySingle
            let acc' = (acc `shiftL` 1) .|. if b then 1 else 0
            go acc' (k - 1)

word8 :: Parser m Word8
word8 = bitsAs 8

word8Exact :: Word8 -> ErrorDescr -> Parser m Word8
word8Exact w8 (ErrorDescr label repr) = do
   v <- word8

   if v == w8
      then return v
      else case repr of
         ReprBin -> fail ("expected `"++printf "%08b" w8++"`, found `"++printf "%08b" v++"` in `"++label++"`")
         ReprHex -> fail ("expected `0x"++printf "%02X" w8++"`, found `0x"++printf "%02X" v++"` in `"++label++"`")
         ReprDec -> fail ("expected `"++show w8++"`, found `"++show v++"` in `"++label++"`")

word16 :: Parser m Word16
word16 = bitsAs 16

word16Exact :: Word16 -> ErrorDescr -> Parser m Word16
word16Exact w16 (ErrorDescr label repr) = do
   v <- word16

   if v == w16
      then return v
      else case repr of
         ReprBin -> fail ("expected `"++printf "%016b" w16++"`, found `"++printf "%016b" v++"` in `"++label++"`")
         ReprHex -> fail ("expected `0x"++printf "%04X" w16++"`, found `0x"++printf "%04X" v++"` in `"++label++"`")
         ReprDec -> fail ("expected `"++show w16++"`, found `"++show v++"` in `"++label++"`")

word32 :: Parser m Word32
word32 = bitsAs 32

word32Exact :: Word32 -> ErrorDescr -> Parser m Word32
word32Exact w32 (ErrorDescr label repr) = do
   v <- word32

   if v == w32
      then return v
      else case repr of
         ReprBin -> fail ("expected `"++printf "%032b" w32++"`, found `"++printf "%032b" v++"` in `"++label++"`")
         ReprHex -> fail ("expected `0x"++printf "%08X" w32++"`, found `0x"++printf "%08X" v++"` in `"++label++"`")
         ReprDec -> fail ("expected `"++show w32++"`, found `"++show v++"` in `"++label++"`")

word64 :: Parser m Word64
word64 = bitsAs 64

word64Exact :: Word64 -> ErrorDescr -> Parser m Word64
word64Exact w64 (ErrorDescr label repr) = do
   v <- word64

   if v == w64
      then return v
      else case repr of
         ReprBin -> fail ("expected `"++printf "%064b" w64++"`, found `"++printf "%064b" v++"` in `"++label++"`")
         ReprHex -> fail ("expected `0x"++printf "%016X" w64++"`, found `0x"++printf "%016X" v++"` in `"++label++"`")
         ReprDec -> fail ("expected `"++show w64++"`, found `"++show v++"` in `"++label++"`")

skipLastBits :: Int -> Parser m ()
skipLastBits n = updateParserState $ \st ->
   let BitStream v = stateInput st
       v' = if n > 0 && n < U.length v
            then U.slice 0 (U.length v - n) v
            else v
   in st { stateInput = BitStream v' }

bitToStr :: Bit -> String
bitToStr b = case b of
   Bit True  -> "1"
   Bit False -> "0"

bitsToStr :: U.Vector Bit -> String
bitsToStr = U.foldr (\b acc -> bitToStr b ++ acc) ""

bsToBitStream :: BS.ByteString -> BitStream
bsToBitStream = BitStream <$> cloneFromByteString . BS.map rev8

rev8 :: Word8 -> Word8
rev8 x =
   let x1 = ((x .&. 0xF0) `shiftR` 4) .|. ((x .&. 0x0F) `shiftL` 4)
       x2 = ((x1 .&. 0xCC) `shiftR` 2) .|. ((x1 .&. 0x33) `shiftL` 2)
   in       ((x2 .&. 0xAA) `shiftR` 1) .|. ((x2 .&. 0x55) `shiftL` 1)

(<->) :: Parser m a -> Parser m a -> Parser m a
pa <-> pb = do
   a <- observing $ try pa
   case a of
      Right r   -> return r
      Left errA -> do
         b <- observing pb
         case b of
            Right r   -> return r
            Left errB -> parseError (errA <> errB)