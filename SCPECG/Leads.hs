module SCPECG.Leads (SCPLeads) where

import Data.ByteString.Lazy (ByteString)
import Data.Binary.Get ( Get, runGet, isEmpty, isolate, skip
                       , getWord8, getWord16le, getWord32le
                       , getByteString, getRemainingLazyByteString)
import Data.Word (Word8, Word16, Word32)
import Data.Bits (Bits(..), testBit)

import SCPECG.Types

data SCPLeads = SCPLeads { scpLeadsNum :: Word8
                         , scpLeadsRefSubUsed :: Bool
                         , scpLeadsSimulRec :: Bool
                         , scpLeadsList :: [(Word8, Word32, Word32)]
                         , scpLeadsRest :: ByteString
                         } deriving Show

instance SCPSection SCPLeads where
--parseSection :: Integer -> Word16 -> Get (Either String SCPLeads)
  parseSection size _ = do
    skip 16
    nleads <- getWord8
    flag <- getWord8
    let refsub = testBit flag 0
    let simrec = testBit flag 2
    dat <- getList nleads
    rest <- getRemainingLazyByteString
    return $ Right $ SCPLeads nleads refsub simrec dat rest
    where
      getList :: Word8 -> Get [(Word8, Word32, Word32)]
      getList n = do
        empty <- isEmpty
        if empty || (n == 0)
          then return []
          else do
            beg <- getWord32le
            end <- getWord32le
            lid <- getWord8
            dat' <- getList (n - 1)
            return $ (lid, beg, end):dat'

instance Semigroup SCPLeads where
  x <> y = x
