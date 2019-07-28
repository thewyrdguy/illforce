module SCPECG.Hufftabs (SCPHufftabs) where

import Data.ByteString.Lazy (ByteString)
import Data.Binary.Get ( Get, runGet, isEmpty, isolate, skip
                       , getWord8, getWord16le, getWord32le
                       , getByteString, getRemainingLazyByteString)
import Data.Word (Word8, Word16, Word32)

import SCPECG.Types

data SCPHufftabs = SCPHufftabs { scpHuffNTables :: Word16
                               , scpHuffData :: [Word8]
                               } deriving Show

instance SCPSection SCPHufftabs where
--parseSection :: Integer -> Word16 -> Get (Either String SCPHufftabs)
  parseSection size _ = do
    skip 16
    ntabs <- getWord16le
    -- if ntabs == 19999 there will be no data, use default tables.
    -- if ntabs > 0, the first two bytes in the table is
    -- "Number of code structures in table"
    -- But we do not know what to do with them.
    -- Experimentally, I saw 1 table with 1 "code structure" and there
    -- where 10 bytes there: [0,16,1,0,0,0,8,0,0,0]
    dat <- getDat
    return $ Right $ SCPHufftabs ntabs dat
    where
      getDat :: Get [Word8]
      getDat = do
        empty <- isEmpty
        if empty
          then return []
          else do
            byte <- getWord8
            dat' <- getDat
            return $ byte:dat'
