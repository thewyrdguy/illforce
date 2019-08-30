module SCPECG.RefBeats (SCPRefBeats) where

import Data.ByteString.Lazy (ByteString)
import Data.Binary.Get ( Get, runGet, isEmpty, isolate, skip
                       , getWord8, getWord16le, getWord32le
                       , getByteString, getRemainingLazyByteString)
import Data.Word (Word8, Word16, Word32)
import Data.Bits (Bits(..), testBit)

import SCPECG.Types

data SCPRefBeats = SCPRefBeats { scpRefBeatsRest :: ByteString
                             } deriving Show

instance SCPSection SCPRefBeats where
--parseSection :: Integer -> Word16 -> Get (Either String SCPRefBeats)
  parseSection size _ = do
    skip 16
    rest <- getRemainingLazyByteString
    return $ Right $ SCPRefBeats rest

instance Mergeable SCPRefBeats where
  maybeAppend x y = Right x
