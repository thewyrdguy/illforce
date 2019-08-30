module SCPECG.QRSLocs (SCPQRSLocs) where

import Data.ByteString.Lazy (ByteString)
import Data.Binary.Get ( Get, runGet, isEmpty, isolate, skip
                       , getWord8, getWord16le, getWord32le
                       , getByteString, getRemainingLazyByteString)
import Data.Word (Word8, Word16, Word32)
import Data.Bits (Bits(..), testBit)

import SCPECG.Types

data SCPQRSLocs = SCPQRSLocs { scpQRSLocsRest :: ByteString
                             } deriving Show

instance SCPSection SCPQRSLocs where
--parseSection :: Integer -> Word16 -> Get (Either String SCPQRSLocs)
  parseSection size _ = do
    skip 16
    rest <- getRemainingLazyByteString
    return $ Right $ SCPQRSLocs rest

instance Mergeable SCPQRSLocs where
  maybeAppend x y = Right x
