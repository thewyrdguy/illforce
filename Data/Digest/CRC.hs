{- | crc16 ccitt

   1) MSB first 1021 x^16 + x^12 + x^5 + 1 

   2) LSB first 8048

   This source code is borrowed from BSD3 licensed "crc16" package
   by Joris Putcuyps. Becase the package is not included in any major
   curated repository, it is easier to copy the source than to
   introduce dependency. Poly specific wrapper added, and package
   renamed as a precausion to avoid name clash.

-}
module Data.Digest.CRC (crc16Update) where

import Prelude hiding (foldl')
import Data.ByteString hiding (foldl)
import Data.Word(Word8,Word16)
import Data.Bits

{-
Usage:

crc16 :: Word16 -> Bool -> Word16 -> ByteString -> Word16
crc16 poly inverse initial =
  foldl' (crc16Update poly inverse) initial

crc16ccitt ::  ByteString -> Word16
crc16ccitt = crc16 0x1021 False 0xffff
-}

-- | crc16 calculation
-- This uses the simple method based on /bit shifting/.
-- See the unittests for an example.
--
crc16Update :: Word16      -- ^ polynomial
            -> Bool        -- ^ inverse bits
            -> Word16      -- ^ initial crc
            -> Word8       -- ^ data byte
            -> Word16      -- ^ new crc
crc16Update poly rev crc b =
    foldl (crc16UpdateBit poly) new_crc [1..(finiteBitSize b)]
    where
        new_crc = crc `xor` shiftL (fromIntegral b' :: Word16) 8
        b' = if rev
                then reverseBits b
                else b




crc16UpdateBit :: Word16 -> Word16 -> Int -> Word16
crc16UpdateBit poly crc _ =
    if (crc .&. 0x8000) /= 0x0000
        then shiftL crc 1 `xor` poly
        else shiftL crc 1



-- | Reverse the bits in a byte
--
-- 7..0 becomes 0..7
--
reverseBits :: Word8 -> Word8
reverseBits b =
  shiftL (b .&. 0x01) 7
  .|. shiftL (b .&. 0x02) 5
  .|. shiftL (b .&. 0x04) 3
  .|. shiftL (b .&. 0x08) 1
  .|. shiftR (b .&. 0x10) 1
  .|. shiftR (b .&. 0x20) 3
  .|. shiftR (b .&. 0x40) 5
  .|. shiftR (b .&. 0x80) 7

