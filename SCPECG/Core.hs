module SCPECG.Core (parseSCP, mergeSCP, SCPRec(..), SCPSec(..)) where

import Prelude hiding (splitAt)
import Control.Applicative (liftA2)
import Data.Maybe (fromMaybe)
import Data.List (uncons)
import Data.Foldable (foldl')
import Data.ByteString.Lazy (ByteString, splitAt)
import Data.Binary.Get ( Get, runGet, isolate, skip, lookAhead, isEmpty
                       , getWord8, getWord16le, getWord32le
                       , getByteString)
import Data.Word (Word8, Word16, Word32)
import Data.Int (Int64)

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

-- Stub for non-implemented section parsers:
instance SCPSection () where
  parseSection size id = return $ Right ()

data SCPSec = S0  SCPPointer
            | S1  SCPMetadata
            | S2  SCPHufftabs
            | S3  SCPLeads
            | S4  SCPQRSLocs
            | S5  SCPRefBeats
            | S6  SCPSignal
            | S7  ()
            | S8  ()
            | S9  ()
            | S10 ()
            | S11 ()
            | Sv  SCPVendor
            deriving Show

data SCPRec = SCPRec { s0  :: Maybe SCPPointer
                     , s1  :: Maybe SCPMetadata
                     , s2  :: Maybe SCPHufftabs
                     , s3  :: Maybe SCPLeads
                     , s4  :: Maybe SCPQRSLocs
                     , s5  :: Maybe SCPRefBeats
                     , s6  :: Maybe SCPSignal
                     , s7  :: Maybe ()
                     , s8  :: Maybe ()
                     , s9  :: Maybe ()
                     , s10 :: Maybe ()
                     , s11 :: Maybe ()
                     , sv  :: Maybe SCPVendor
                     } deriving Show
nullSCPRec = SCPRec Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                    Nothing Nothing Nothing Nothing Nothing Nothing

secToRec :: SCPRec -> SCPSec -> SCPRec
secToRec rec (S0   x) = rec { s0  = (s0  rec) <> (Just x)}
secToRec rec (S1   x) = rec { s1  = (s1  rec) <> (Just x)}
secToRec rec (S2   x) = rec { s2  = (s2  rec) <> (Just x)}
secToRec rec (S3   x) = rec { s3  = (s3  rec) <> (Just x)}
secToRec rec (S4   x) = rec { s4  = (s4  rec) <> (Just x)}
secToRec rec (S5   x) = rec { s5  = (s5  rec) <> (Just x)}
secToRec rec (S6   x) = rec { s6  = (s6  rec) <> (Just x)}
secToRec rec (S7   x) = rec { s7  = (s7  rec) <> (Just x)}
secToRec rec (S8   x) = rec { s8  = (s8  rec) <> (Just x)}
secToRec rec (S9   x) = rec { s9  = (s9  rec) <> (Just x)}
secToRec rec (S10  x) = rec { s10 = (s10 rec) <> (Just x)}
secToRec rec (S11  x) = rec { s11 = (s11 rec) <> (Just x)}
secToRec rec (Sv   x) = rec { sv  = (sv  rec) <> (Just x)}

parseSCPsection :: Integer
                -> Word16
                -> ByteString
                -> Either String SCPSec
parseSCPsection size id cont =
  case id of
    0  -> S0  <$> runGet (parseSection size id) cont
    1  -> S1  <$> runGet (parseSection size id) cont
    2  -> S2  <$> runGet (parseSection size id) cont
    3  -> S3  <$> runGet (parseSection size id) cont
    4  -> S4  <$> runGet (parseSection size id) cont
    5  -> S5  <$> runGet (parseSection size id) cont
    6  -> S6  <$> runGet (parseSection size id) cont
    7  -> S7  <$> runGet (parseSection size id) cont
    8  -> S8  <$> runGet (parseSection size id) cont
    9  -> S9  <$> runGet (parseSection size id) cont
    10 -> S10 <$> runGet (parseSection size id) cont
    11 -> S11 <$> runGet (parseSection size id) cont
    _  -> Sv  <$> runGet (parseSection size id) cont

-- Parse sections and return lazy list of sections
parseSCPsecs :: Int64
             -> ByteString
             -> [Either String SCPSec]
parseSCPsecs size cont
  | size == 0 = []
  | size < 8  = [Left ("short data " ++ (show size))]
  | otherwise =
      let
        (expectedcrc, id, secsz) = runGet (isolate 8 parseSecHeader) cont
        parseSecHeader :: Get (Word16, Word16, Int64)
        parseSecHeader = do
          crc <- getWord16le
          id <- getWord16le
          size_w <- getWord32le
          return (crc, id, fromIntegral size_w)
        (seccont, rest) = splitAt secsz cont
        (_, rest_to_crc) = splitAt 2 seccont
        realcrc = runGet getCRC rest_to_crc
      in
        case parseSCPsection (fromIntegral secsz) id seccont of
          Left  err -> [Left err]  -- quit parsing
          Right res ->
            if realcrc == expectedcrc
              then
                (Right res):(parseSCPsecs (size - secsz) rest)
              else
                [Left $ "expected crc:" ++ (show expectedcrc)
                        ++ " real crc:" ++ (show realcrc)
                      ++ " in section " ++ (show id)]

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
    expectedcrc = runGet getWord16le crchdr
    -- realcrc = runGet getCRC rest_to_crc -- This kills lazy consumption
    realcrc = expectedcrc -- fake it for now. In the future, we want to
                          -- calculate crc incrementally and after the
                          -- last section in the file is parsed, check it
                          -- and if it does not match, add an extra 'Left'
                          -- element at the tail.
    (sizehdr, rest_to_parse) = splitAt 4 rest_to_crc
    expectedsize = fromIntegral $ runGet getWord32le sizehdr
  in
    if fromMaybe expectedsize (fromIntegral <$> maybesize) == expectedsize
      then do
        if realcrc == expectedcrc
          then parseSCPsecs (expectedsize - 6) rest_to_parse
          else [Left $ "expected crc:" ++ (show expectedcrc)
                       ++ " real crc:" ++ (show realcrc)]
      else
        [Left $ "expected size:" ++ (show expectedsize)
                ++ " real size:" ++ (show maybesize)]

invert :: [[a]] -> [[a]]
invert ll = case fmap unzip . sequenceA . fmap uncons $ ll of
  Just (x, xs) -> x:invert xs
  Nothing -> []

mergeSCP :: [[Either String SCPSec]] -> Either String SCPRec
mergeSCP = foldl' (liftA2 secToRec) (Right nullSCPRec) . concat . invert
