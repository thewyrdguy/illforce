module SCPECG.Core (SCPRecord, parseSCP, mergeSCP) where

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
    -- liftA2 (\r v -> r { s0 = Just v }) rec $ runGet (parseSection size id) cont
    0  -> case runGet (parseSection size id) cont of
            Left err -> Left err
            Right val -> Right $ rec { s0 = Just val }
    1  -> case runGet (parseSection size id) cont of
            Left err -> Left err
            Right val -> Right $ rec { s1 = Just val }
    2  -> case runGet (parseSection size id) cont of
            Left err -> Left err
            Right val -> Right $ rec { s2 = Just val }
    3  -> case runGet (parseSection size id) cont of
            Left err -> Left err
            Right val -> Right $ rec { s3 = Just val }
    4  -> case runGet (parseSection size id) cont of
            Left err -> Left err
            Right val -> Right $ rec { s4 = Just val }
    5  -> case runGet (parseSection size id) cont of
            Left err -> Left err
            Right val -> Right $ rec { s5 = Just val }
    6  -> case runGet (parseSection size id) cont of
            Left err -> Left err
            Right val -> Right $ rec { s6 = Just val }
    _  -> case runGet (parseSection size id) cont of
            Left err -> Left err
            Right val -> Right $ rec { sv = val:(sv rec) }

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

mergeSCP :: [[Either String SCPRecord]] -> [Either String SCPRecord]
mergeSCP ll = head ll  -- TODO: implement merge
