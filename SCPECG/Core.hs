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

data SCPRecord = SCPRecord { s0 :: Maybe SCPPointer
                           , s1 :: Maybe SCPMetadata
                           , s2 :: Maybe SCPHufftabs
                           , s3 :: Maybe SCPLeads
                           , s4 :: Maybe SCPQRSLocs
                           , s5 :: Maybe SCPRefBeats
                           , s6 :: Maybe SCPSignal
                           , s7 :: Maybe ()
                           , s8 :: Maybe ()
                           , s9 :: Maybe ()
                           , s10 :: Maybe ()
                           , s11 :: Maybe ()
                           , sv :: [SCPVendor]
                           } deriving Show
emptyRecord = SCPRecord    Nothing
                           Nothing
                           Nothing
                           Nothing
                           Nothing
                           Nothing
                           Nothing
                           Nothing
                           Nothing
                           Nothing
                           Nothing
                           Nothing
                           []

-- return a new copy of the record with one more section filled
parseSCPsection :: SCPRecord
                -> Integer
                -> Word16
                -> ByteString
                -> Either String SCPRecord
parseSCPsection rec size id cont =
  case id of
    0  -> liftA2 (\r v -> r { s0  = Just v }) (Right rec) $
            runGet (parseSection size id) cont
    1  -> liftA2 (\r v -> r { s1  = Just v }) (Right rec) $
            runGet (parseSection size id) cont
    2  -> liftA2 (\r v -> r { s2  = Just v }) (Right rec) $
            runGet (parseSection size id) cont
    3  -> liftA2 (\r v -> r { s3  = Just v }) (Right rec) $
            runGet (parseSection size id) cont
    4  -> liftA2 (\r v -> r { s4  = Just v }) (Right rec) $
            runGet (parseSection size id) cont
    5  -> liftA2 (\r v -> r { s5  = Just v }) (Right rec) $
            runGet (parseSection size id) cont
    6  -> liftA2 (\r v -> r { s6  = Just v }) (Right rec) $
            runGet (parseSection size id) cont
    7  -> liftA2 (\r v -> r { s7  = Just v }) (Right rec) $
            runGet (parseSection size id) cont
    8  -> liftA2 (\r v -> r { s8  = Just v }) (Right rec) $
            runGet (parseSection size id) cont
    9  -> liftA2 (\r v -> r { s9  = Just v }) (Right rec) $
            runGet (parseSection size id) cont
    10 -> liftA2 (\r v -> r { s10 = Just v }) (Right rec) $
            runGet (parseSection size id) cont
    11 -> liftA2 (\r v -> r { s11 = Just v }) (Right rec) $
            runGet (parseSection size id) cont
    _  -> liftA2 (\r v -> r { sv  = v:(sv r) }) (Right rec) $
            runGet (parseSection size id) cont

-- Parse sections and return lazy list of incrementally filled SCPRecords
parseSCPsecs :: SCPRecord -> Integer -> ByteString -> [Either String SCPRecord]
parseSCPsecs accum size cont
  | size == 0 = []
  | size < 8  = [Left ("short data " ++ (show size))]
  | otherwise =
      let
        (crchdr, rest_to_crc) = splitAt 2 cont
        expectedcrc = runGet getWord16le crchdr
        --realcrc = runGet getCRC rest_to_crc -- TODO
        realcrc = expectedcrc
        (header, rest_to_parse) = splitAt 6 rest_to_crc
        (id, secsz) = runGet parseSecHeader header
        -- parseSecHeader :: Get (Word16, Integer)
        parseSecHeader = do
          id <- getWord16le
          size_w <- getWord32le
          return (id, fromIntegral size_w)
        (seccont, rest) = splitAt (fromIntegral secsz) cont
        result = parseSCPsection accum secsz id seccont
      in
        if realcrc == expectedcrc
          then
            case result of
              Left  _      -> [result]  -- quit parsing
              Right accum' -> result:(parseSCPsecs accum' (size - secsz) rest)
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

parseSCP :: Maybe Integer -> ByteString -> [Either String SCPRecord]
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
          then (Right emptyRecord):(parseSCPsecs emptyRecord
                                      (expectedsize - 6) rest_to_parse)
          else [Left $ "expected crc:" ++ (show expectedcrc)
                       ++ " real crc:" ++ (show realcrc)]
      else
        [Left $ "expected size:" ++ (show expectedsize)
                ++ " real size:" ++ (show maybesize)]

mergeSCP :: [[Either String SCPRecord]] -> Either String SCPRecord
mergeSCP ll = mergeSCP' (Right emptyRecord) ll
  where
  mergeSCP' accum ll =
    case fmap unzip $ sequenceA $ fmap uncons ll of
      -- pick head of each of the sublists:
      -- [[1,2,3],[4,5,6],[7,8,9]] -> Just ([1,4,7],[[2,3],[5,6],[8,9]])
      Nothing  -> accum  -- at the end of at least one of the sublists
      Just (cur, ll') -> mergeSCP' (head cur) ll'  -- TODO: implement merge
