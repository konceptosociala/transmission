{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE QuasiQuotes #-}

module Level.Binary.BitMacro (bits) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Data.Vector.Unboxed as U
import Data.Bit (Bit(..))

bits :: QuasiQuoter
bits = QuasiQuoter
  { quoteExp  = bitsExp
  , quotePat  = \_ -> fail "bits: not usable in patterns"
  , quoteType = \_ -> fail "bits: not usable in types"
  , quoteDec  = \_ -> fail "bits: not usable for declarations"
  }

bitsExp :: String -> Q Exp
bitsExp s =
  [| U.fromList $(listE (map charToBitExp s)) :: U.Vector Bit |]
  where
    charToBitExp '0' = [| Bit False |]  -- :: Q Exp
    charToBitExp '1' = [| Bit True  |]
    charToBitExp c   = fail $ "bits: invalid character '" ++ [c] ++ "' (only 0 or 1 allowed)"
