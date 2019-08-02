module SCPECG.Core (SCPRec, parseSCP, mergeSCP) where

import Prelude hiding (splitAt)
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy (ByteString, splitAt)
import Data.Binary.Get ( Get, runGet, isolate, skip, lookAhead, isEmpty
                       , getWord8, getWord16le, getWord32le
                       , getByteString)
import Data.Word (Word8, Word16, Word32)

import Data.Digest.CRC

import SCPECG.Pointer
import SCPECG.Metadata
import SCPECG.Hufftabs
import SCPECG.Leads
import SCPECG.QRSLocs
import SCPECG.RefBeats
import SCPECG.Signal
import SCPECG.Vendor
import SCPECG.Types

data SCPSec = S0 SCPPointer
            | S1 SCPMetadata
            | S2 SCPHufftabs
            | S3 SCPLeads
            | S4 SCPQRSLocs
            | S5 SCPRefBeats
            | S6 SCPSignal
            | Sv SCPVendor
              deriving Show

data SCPRec = SCPRec [[Either String SCPSec]] deriving Show

parseSCPsection :: Integer -> Word16 -> ByteString -> Either String SCPSec
parseSCPsection size id cont =
  case id of
    0  -> S0 <$> (run parseSection :: Either String SCPPointer)
    1  -> S1 <$> (run parseSection :: Either String SCPMetadata)
    2  -> S2 <$> (run parseSection :: Either String SCPHufftabs)
    3  -> S3 <$> (run parseSection :: Either String SCPLeads)
    4  -> S4 <$> (run parseSection :: Either String SCPQRSLocs)
    5  -> S5 <$> (run parseSection :: Either String SCPRefBeats)
    6  -> S6 <$> (run parseSection :: Either String SCPSignal)
    _  -> Sv <$> (run parseSection :: Either String SCPVendor)
  where
    run parser = runGet (isolate (fromIntegral size) (parser size id)) cont

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

getCRC :: Get Word16
getCRC = getCRCb 0xffff
  where
    getCRCb crc = do
      eod <- isEmpty
      if eod
        then return crc
        else do
          b <- getWord8
          let crc' = crc16Update 0x1021 False crc b
          getCRCb crc'

parseSCP :: Maybe Integer -> ByteString -> [Either String SCPSec]
parseSCP maybesize cont =
  let
    (crchdr, rest_to_crc) = splitAt 2 cont
    expectedcrc = runGet getWord16le crchdr -- This kills lazy consumption
    realcrc = runGet getCRC rest_to_crc
    (sizehdr, rest_to_parse) = splitAt 4 rest_to_crc
    expectedsize = fromIntegral $ runGet getWord32le sizehdr
  in
    if (fromMaybe expectedsize maybesize) == expectedsize
      then do
        if realcrc == expectedcrc
          then parseSCPsecs (expectedsize - 6) rest_to_parse
          else [Left $ "expected csum:" ++ (show expectedcrc)
                       ++ " real csum:" ++ (show realcrc)]
      else
        [Left $ "expected size:" ++ (show expectedsize)
                ++ " real size:" ++ (show maybesize)]

mergeSCP :: [[Either String SCPSec]] -> Either String SCPRec
mergeSCP ll = Right $ SCPRec ll  -- TODO: implement merge
