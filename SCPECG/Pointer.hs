module SCPECG.Pointer (SCPPointer) where

import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)
import Data.Binary.Get ( Get, runGet, isolate, skip
                       , getWord8, getWord16le, getWord32le
                       , getByteString)
import Data.Word (Word8, Word16, Word32)

import SCPECG.Types

data SCPPointer = SCPPointer { scpPointerMagic :: ByteString
                             } deriving Show

instance SCPSection SCPPointer where
--parseSection :: Integer -> Word16 -> Get (Either String SCPPointer)
  parseSection size _ = do
    dat <- getByteString (fromIntegral size)
    return $ Right $ SCPPointer (pack "pOiNteR")
