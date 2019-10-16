module SCPECG.Core (SCPRecord, parseSCP, mergeSCP, SCPSec(..)) where

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

instance Mergeable () where
  maybeAppend () () = Right ()

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

instance Mergeable SCPSec where
  (S0  x) `maybeAppend` (S0  y) = fmap S0  (x `maybeAppend` y)
  (S1  x) `maybeAppend` (S1  y) = fmap S1  (x `maybeAppend` y)
  (S2  x) `maybeAppend` (S2  y) = fmap S2  (x `maybeAppend` y)
  (S3  x) `maybeAppend` (S3  y) = fmap S3  (x `maybeAppend` y)
  (S4  x) `maybeAppend` (S4  y) = fmap S4  (x `maybeAppend` y)
  (S5  x) `maybeAppend` (S5  y) = fmap S5  (x `maybeAppend` y)
  (S6  x) `maybeAppend` (S6  y) = fmap S6  (x `maybeAppend` y)
  (S7  x) `maybeAppend` (S7  y) = fmap S7  (x `maybeAppend` y)
  (S8  x) `maybeAppend` (S8  y) = fmap S8  (x `maybeAppend` y)
  (S9  x) `maybeAppend` (S9  y) = fmap S9  (x `maybeAppend` y)
  (S10 x) `maybeAppend` (S10 y) = fmap S10 (x `maybeAppend` y)
  (S11 x) `maybeAppend` (S11 y) = fmap S10 (x `maybeAppend` y)
  (Sv  x) `maybeAppend` (Sv  y) = fmap Sv  (x `maybeAppend` y)
  x       `maybeAppend` y       = Left $ "Concatenation of different types "
                                       ++ (show x) ++ " and " ++ (show y)

type SCPRecord = [Either String SCPSec]

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

mergeSCP :: [[Either String SCPSec]]
         -> [Either String SCPSec]
mergeSCP ll =
  case fmap unzip $ sequenceA $ fmap uncons ll of
    -- pick head of each of the sublists:
    -- [[1,2,3],[4,5,6],[7,8,9]] -> Just ([1,4,7],[[2,3],[5,6],[8,9]])
    Nothing  -> []  -- at the end of at least one of the sublists
    Just (cur, ll') -> (foldr1 combine cur):(mergeSCP ll')
    where
      combine (Left ex) (Left ey) = Left $ ex ++ "\n" ++ ey
      combine (Left ex) (Right y) = Left ex
      combine (Right x) (Left ey) = Left ey
      combine (Right x) (Right y) = maybeAppend x y
