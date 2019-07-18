import System.Environment (getArgs)
import System.IO (Handle, IOMode(ReadMode), withBinaryFile, hSetBinaryMode
                 , hFileSize, stdin)
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy (hGetContents)
import Data.Binary.Get (Get, runGet, getWord8, getWord16le, getWord32le)
import Data.Word (Word8, Word16, Word32)

type Sec11 = [Int]

data SCPRec = SCPRec {sec11 :: Maybe Sec11
                     } deriving Show
emptySCPRec = SCPRec {sec11 = Nothing}

skip :: Integer -> Get ()
skip 0 = return ()
skip n = getWord8 >> skip (n - 1)

parseSCPsection :: SCPRec -> Integer -> Word16
                -> Get (Either String (SCPRec, Integer, Word16))
parseSCPsection scprec remains crc
  | remains == 0 = return $! Right (scprec, 0, 17652)
  | remains < 8  = return $! Left ("short data " ++ (show remains))
  | otherwise = do
    seccrc <- getWord16le
    sec_id <- getWord16le
    seclen <- getWord32le
    skip ((fromIntegral seclen) - 8)
    let newrec = SCPRec {sec11 =
      Just ((fromIntegral sec_id)
           :(fromIntegral seclen)
           :(fromMaybe [] (sec11 scprec)))
      }
    parseSCPsection newrec (remains - (fromIntegral seclen)) crc

parseSCPfile :: Integer -> Get (Either String SCPRec)
parseSCPfile realsize = do
  expectedcrc <- getWord16le
  expectedsize <- getWord32le
  if realsize == (fromIntegral expectedsize) then do
      parseout <- parseSCPsection emptySCPRec (realsize - 6) 0
      case parseout of
        Right (scprec, _, realcrc) ->
          if realcrc == expectedcrc then
              return $ Right scprec
            else
              return $ Left $ "expected csum:" ++ (show expectedcrc)
                                   ++ " real:" ++ (show realcrc)
        Left err -> return $ Left err
    else
      return $ Left $ "csum:" ++ (show expectedcrc)
                  ++ " expected:" ++ (show expectedsize)
                  ++ " real:" ++ (show realsize)

readSCP :: Handle -> IO (Either String SCPRec)
readSCP fh = do
  realsize <- hFileSize fh
  contents <- hGetContents fh
  -- TODO: when the application is not strict ($ instead of $!),
  -- the following runtime error happens:
  -- *** Exception: xxx.scp: hGetBufSome: illegal operation (handle is closed)
  -- Need to figure out how to stay lazy and avoid this error.
  return $! runGet (parseSCPfile realsize) contents

main = do
  args <- getArgs
  scprec <- case args of
    []    -> hSetBinaryMode stdin True >> readSCP stdin
    fn:xs -> withBinaryFile fn ReadMode $ \fh -> readSCP fh
  print scprec
