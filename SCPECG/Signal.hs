module SCPECG.Signal (SCPSignal(..)) where

import Data.ByteString.Lazy (ByteString)
import Data.Binary.Get ( Get, runGet, isEmpty, isolate, skip
                       , getWord8, getWord16le, getWord32le
                       , getByteString, getRemainingLazyByteString)
import Data.Word (Word8, Word16, Word32)

import SCPECG.Types

data SCPSignal = SCPSignal { scpSignalnVper1 :: Word16
                           , scpSignaluSper1 :: Word16
                           , scpSignalEncType :: Word8
                           , scpSignalCprType :: Word8
                           , scpSignalLeadLen :: Word16
                           , scpSignalDataLen :: Int
                           , scpSignalData :: [Word16]
                           } deriving Show

instance SCPSection SCPSignal where
--parseSection :: Integer -> Word16 -> Get (Either String SCPSignal)
  parseSection size _ = do
    skip 16
    mult <- getWord16le
    period <- getWord16le
    enc <- getWord8
    comp <- getWord8
--    skip 2
    leadl <- getWord16le
    dat <- getDat
    let len = length dat
    return $ Right $ SCPSignal mult period enc comp leadl len dat
    where
      getDat :: Get [Word16]
      getDat = do
        empty <- isEmpty
        if empty
          then return []
          else do
            word <- getWord16le
            dat' <- getDat
            return $ word:dat'

instance Mergeable SCPSignal where
  maybeAppend x y
    | (scpSignalnVper1 x)  /= (scpSignalnVper1 y)  = Left "Vper1 mismatch"
    | (scpSignaluSper1 x)  /= (scpSignaluSper1 y)  = Left "Sper1 mismatch"
    | (scpSignalEncType x) /= (scpSignalEncType y) = Left "EncType mismatch"
    | (scpSignalCprType x) /= (scpSignalCprType y) = Left "CprType mismatch"
    | (scpSignalLeadLen x) /= (scpSignalLeadLen y) = Left "Leadl mismatch"
    | otherwise =
        Right $ x { scpSignalLeadLen = (scpSignalLeadLen x + scpSignalLeadLen y)
                  , scpSignalDataLen = (scpSignalDataLen x + scpSignalDataLen y)
                  , scpSignalData = (scpSignalData x ++ scpSignalData y)
                  }
