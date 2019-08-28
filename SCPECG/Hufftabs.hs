module SCPECG.Hufftabs ( SCPHufftabs(..)
                       , SCPHuffTable(..)
                       , SCPHuffCodeStruct(..)
                       ) where

import Control.Monad (replicateM)
import Data.ByteString.Lazy (ByteString)
import Data.Binary.Get ( Get, runGet, isEmpty, isolate, skip
                       , getWord8, getWord16le, getWord32le
                       , getByteString, getRemainingLazyByteString)
import Data.Word (Word8, Word16, Word32)

import SCPECG.Types

data SCPHuffCodeStruct = SCPHuffCodeStruct { scpHuffCodeBitsInPrefix :: Word8
                                           , scpHuffCodeBitsInCode   :: Word8
                                           , scpHuffCodeModeSwitch   :: Word8
                                           , scpHuffCodeBaseValue    :: Word16
                                           , scpHuffCodeBaseCode     :: Word32
                                           } deriving Show

data SCPHuffTable = SCPHuffTable { scpHuffCodeStructsNum :: Word16
                                 , scpHuffCodeStructs :: [SCPHuffCodeStruct]
                                 } deriving Show

data SCPHufftabs = SCPHufftabs { scpHuffTablesNum :: Word16
                               , scpHuffTables  :: [SCPHuffTable]
                               } deriving Show

instance SCPSection SCPHufftabs where
--parseSection :: Integer -> Word16 -> Get (Either String SCPHufftabs)
  parseSection size _ = do
    skip 16
    ntabs <- getWord16le
    if ntabs == 19999
      then return $ Right $ SCPHufftabs ntabs []
      else do
        tables <- replicateM (fromIntegral ntabs) getTable
        return $ Right $ SCPHufftabs ntabs tables
    where
      getTable :: Get SCPHuffTable
      getTable = do
        numstructs  <- getWord16le
        codestructs <- replicateM (fromIntegral numstructs) getCode
        return $ SCPHuffTable numstructs codestructs
      getCode :: Get SCPHuffCodeStruct
      getCode = do
        bitsInPfx  <- getWord8
        bitsInCode <- getWord8
        modeSwitch <- getWord8
        baseValue  <- getWord16le
        baseCode   <- getWord32le
        return $ SCPHuffCodeStruct bitsInPfx bitsInCode modeSwitch
                 baseValue baseCode

instance Semigroup SCPHufftabs where
  x <> y = x
