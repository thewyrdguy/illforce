import System.Environment (getArgs)
import System.IO (Handle, IOMode(ReadMode), withBinaryFile, hSetBinaryMode
                 , hFileSize, stdin)
import Data.ByteString.Lazy (hGetContents)
import Data.Binary.Get (Get, runGet, getWord8, getWord16le, getWord32le)
import Data.Word (Word8, Word16, Word32)

type Sec11 = [Int]

data SCPRec = SCPRec {sec11 :: Maybe Sec11
                     } deriving Show

readSCP :: Handle -> IO (Either String SCPRec)
readSCP fh = do
  realsize <- hFileSize fh
  print realsize
  contents <- hGetContents fh
  print $ take 40 (show contents)
  return $ runGet parseSCP contents
  where
    parseSCP :: Get (Either String SCPRec)
    parseSCP = do
      expectedcrc <- getWord16le
      expectedsize <- getWord32le
      -- when realsize <> expectedsize $ error "bad size"
      return $! Left $ "CRC " ++ (show expectedcrc) ++ " - " ++ (show expectedsize)

main = do
  args <- getArgs
  case args of
    []    -> hSetBinaryMode stdin True >> readSCP stdin
    fn:xs -> withBinaryFile fn ReadMode $ \fh -> readSCP fh
