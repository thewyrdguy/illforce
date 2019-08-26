module SCPECG.Core (SCPRecord, parseSCP, mergeSCP) where

import Prelude hiding (splitAt)
import Control.Applicative (liftA2)
import Data.Maybe (fromMaybe)
import Data.List (uncons)
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

type SCPRecord = [(Word16, Either String SCPSec)]

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
             -> [(Word16, Either String SCPSec)]
parseSCPsecs size cont
  | size == 0 = []
  | size < 8  = [(999, Left ("short data " ++ (show size)))]
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
          Left  err -> [(999, Left err)]  -- quit parsing
          Right res ->
            if realcrc == expectedcrc
              then
                (id, Right res):(parseSCPsecs (size - secsz) rest)
              else
                [(999, Left $ "expected crc:" ++ (show expectedcrc)
                              ++ " real crc:" ++ (show realcrc)
                            ++ " in section " ++ (show id))]

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

parseSCP :: Maybe Integer -> ByteString -> [(Word16, Either String SCPSec)]
parseSCP maybesize cont =
  let
    (crchdr, rest_to_crc) = splitAt 2 cont
    expectedcrc = runGet getWord16le crchdr -- This kills lazy consumption
    realcrc = runGet getCRC rest_to_crc
    (sizehdr, rest_to_parse) = splitAt 4 rest_to_crc
    expectedsize = fromIntegral $ runGet getWord32le sizehdr
  in
    if fromMaybe expectedsize (fromIntegral <$> maybesize) == expectedsize
      then do
        if realcrc == expectedcrc
          then parseSCPsecs (expectedsize - 6) rest_to_parse
          else [(999, Left $ "expected crc:" ++ (show expectedcrc)
                             ++ " real crc:" ++ (show realcrc))]
      else
        [(999, Left $ "expected size:" ++ (show expectedsize)
                      ++ " real size:" ++ (show maybesize))]

mergeSCP :: [[(Word16, Either String SCPSec)]]
         -> [(Word16, Either String SCPSec)]
mergeSCP ll =
  case fmap unzip $ sequenceA $ fmap uncons ll of
    -- pick head of each of the sublists:
    -- [[1,2,3],[4,5,6],[7,8,9]] -> Just ([1,4,7],[[2,3],[5,6],[8,9]])
    Nothing  -> []  -- at the end of at least one of the sublists
    Just (cur, ll') -> (mergeSec (unzip cur)):(mergeSCP ll')
    where
      mergeSec :: ([Word16], [Either String SCPSec])
               -> (Word16, Either String SCPSec)
      mergeSec (ids, members)
        | all (== head ids) (tail ids) =  -- all IDs are the same
          (head ids, head members)  -- TODO: implement merge
        | otherwise = (999, Left ("Secion ID mismatch: " ++ (show ids)))
