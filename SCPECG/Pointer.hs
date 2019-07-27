module SCPECG.Pointer (SCPPointer) where

import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)
import Data.Binary.Get ( Get, runGet, isEmpty, isolate, skip
                       , getWord8, getWord16le, getWord32le
                       , getByteString)
import Data.Word (Word8, Word16, Word32)

import SCPECG.Types

data SCPPointer = SCPPointer { scpPointerIdx :: [(Word16, Word32, Word32)]
                             } deriving Show

instance SCPSection SCPPointer where
--parseSection :: Integer -> Word16 -> Get (Either String SCPPointer)
  parseSection size _ = do
    skip 8
    pfx <- getByteString 8
    idx <- getIdx
    return $ if pfx == (pack "\r\rSCPECG")
      then Right $ SCPPointer idx
      else Left $ "Unexpected magic string: " ++ (show pfx)
    where
      getIdx :: Get [(Word16, Word32, Word32)]
      getIdx = do
        empty <- isEmpty
        if empty
          then return []
          else do
            id <- getWord16le
            len <- getWord32le
            off <- getWord32le
            idx' <- getIdx
            return $ (id, off, len):idx'
