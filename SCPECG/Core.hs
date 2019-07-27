module SCPECG.Core (SCPRec, parseSCP, mergeSCP) where

import Prelude hiding (splitAt)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B (ByteString, empty)
import Data.ByteString.Lazy (ByteString, splitAt)
import Data.Binary.Get ( Get, runGet, isolate, skip
                       , getWord8, getWord16le, getWord32le
                       , getByteString)
import Data.Word (Word8, Word16, Word32)

import SCPECG.Types

data Vendor = Vendor { scpVendorSize :: Integer
                     , scpVendorId   :: Word16
                     , scpVendorData :: B.ByteString
                     } deriving Show

instance SCPSection Vendor where
--parseSection :: Integer -> Word16 -> Get (Either String Vendor)
  parseSection size id = do
    dat <- getByteString (fromIntegral size)
    return $ Right $ Vendor size id B.empty -- dat

data SCPSec = S0 Int
            | Sv Vendor
              deriving Show

data SCPRec = SCPRec [[Either String SCPSec]] deriving Show

parseSCPsection :: Integer -> Word16 -> ByteString -> Either String SCPSec
parseSCPsection size id cont =
  let
    parser = case id of
      _  -> parseSection :: Integer -> Word16 -> Get (Either String Vendor)
  in
    fmap Sv $ runGet (isolate (fromIntegral size) (parser size id)) cont

parseSCPsecs :: Integer -> ByteString -> [Either String SCPSec]
parseSCPsecs size cont
  | size == 0 = []
  | size < 8  = [Left ("short data " ++ (show size))]
  | otherwise =
      let
        (header, rest) = splitAt 8 cont
        (expectedcrc, id, secsz) = runGet (isolate 8 parseSecHeader) header
        realcrc = expectedcrc -- TODO: realcrc = calccrc(rest)
        parseSecHeader :: Get (Word16, Word16, Integer)
        parseSecHeader = do
          crc <- getWord16le
          id <- getWord16le
          size_w <- getWord32le
          return (crc, id, fromIntegral size_w)
        (seccont, cont') = splitAt (fromIntegral secsz) cont
      in
        (parseSCPsection secsz id seccont):(parseSCPsecs (size - secsz) cont')

parseSCP :: Maybe Integer -> ByteString -> [Either String SCPSec]
parseSCP maybesize cont =
  let
    (header, rest) = splitAt 6 cont
    (expectedcrc, expectedsize) = runGet (isolate 6 parseFileHeader) header
    realcrc = expectedcrc -- TODO: realcrc = calccrc(rest)
    parseFileHeader :: Get (Word16, Integer)
    parseFileHeader = do
      crc <- getWord16le
      size_w <- getWord32le
      return (crc, fromIntegral size_w)
  in
    if (fromMaybe expectedsize maybesize) == expectedsize
      then
        if realcrc == expectedcrc
          then parseSCPsecs (expectedsize - 6) rest
          else [Left $ "expected csum:" ++ (show expectedcrc)
                       ++ " real csum:" ++ (show realcrc)]
      else
          [Left $ "expected size:" ++ (show expectedsize)
                  ++ " real size:" ++ (show maybesize)]

mergeSCP :: [[Either String SCPSec]] -> Either String SCPRec
mergeSCP ll = Right $ SCPRec ll  -- TODO: implement merge
