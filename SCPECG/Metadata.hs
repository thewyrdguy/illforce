module SCPECG.Metadata (SCPMetadata) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Binary.Get ( Get, runGet, isEmpty, isolate, skip
                       , getWord8, getWord16le, getWord32le
                       , getByteString)
import Data.Word (Word8, Word16, Word32)
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Time.Calendar(Day, fromGregorian)

import SCPECG.Types

data SCPMetadata = SCPMetadata { scpMetaUndecoded :: [(Word8, Word16, ByteString)]
                               , scpMetaLastName :: Maybe String -- 0
                               , scpMetaDay :: Maybe Day -- 25
                               , scpMetaTime :: Maybe TimeOfDay -- 26
                           } deriving (Show)
emptyMeta = SCPMetadata [] Nothing Nothing Nothing
-- NB: nuiscance, but more portable than using generics

instance SCPSection SCPMetadata where
--parseSection :: Integer -> Word16 -> Get (Either String SCPMetadata)
  parseSection size _
    | size < 19 = return $ Left $ "Metadata section too short: " ++ (show size)
    | otherwise = do
      skip 16
      rec <- getTags emptyMeta
      return $ Right $ rec
      where
        getTags :: SCPMetadata -> Get SCPMetadata
        getTags accum = do
          empty <- isEmpty
          if empty
            then return accum
            else do
              id <- getWord8
              len <- getWord16le
              accum' <- case id of
                0 -> do
                  bstr <- getByteString (fromIntegral len)
                  return accum { scpMetaLastName = Just (unpack bstr)}
                25 -> do
                  y <- getWord16le
                  m <- getWord8
                  d <- getWord8
                  return accum { scpMetaDay = Just (fromGregorian
                        (fromIntegral y) (fromIntegral m) (fromIntegral d))}
                26 -> do
                  h <- getWord8
                  m <- getWord8
                  s <- getWord8
                  return accum { scpMetaTime = Just (TimeOfDay
                        (fromIntegral h) (fromIntegral m) (fromIntegral s))}
                255 -> return accum  -- length _must_ be zero
                otherwise -> do
                  dat <- getByteString (fromIntegral len)
                  return accum { scpMetaUndecoded =
                                 (id, len, dat):(scpMetaUndecoded accum)}
              getTags accum'

instance Semigroup SCPMetadata where
  x <> y = x
