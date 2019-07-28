module SCPECG.Vendor (SCPVendor) where

import Data.ByteString (ByteString)
import Data.Binary.Get ( Get, runGet, isolate, skip
                       , getWord8, getWord16le, getWord32le
                       , getByteString)
import Data.Word (Word8, Word16, Word32)

import SCPECG.Types

data SCPVendor = SCPVendor { scpVendorSize :: Integer
                           , scpVendorId   :: Word16
                           , scpVendorData :: ByteString
                           }

instance Show SCPVendor where
  show v = "SCPVendor { scpVendorSize=" ++ (show (scpVendorSize v))
                    ++ ", scpVendorId=" ++ (show (scpVendorId v))
                    ++ ", scpVendorData=<not shown>"

instance SCPSection SCPVendor where
--parseSection :: Integer -> Word16 -> Get (Either String SCPVendor)
  parseSection size id = do
    dat <- getByteString (fromIntegral size)
    return $ Right $ SCPVendor size id dat