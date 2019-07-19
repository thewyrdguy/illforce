import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), openBinaryFile, hSetBinaryMode
                 ,hIsSeekable, hFileSize, stdin)
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
  | remains == 0 = return $! Right (scprec, 0, 0)
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

parseSCPfile :: Maybe Integer -> Get (Either String SCPRec)
parseSCPfile realsize = do
  expectedcrc <- getWord16le
  expectedsize_w <- getWord32le
  let expectedsize = (fromIntegral expectedsize_w)
  if (fromMaybe expectedsize realsize) == expectedsize then do
      parseout <- parseSCPsection emptySCPRec (expectedsize - 6) 0
      case parseout of
        Right (scprec, _, realcrc) ->
          -- TODO: calculate and compare checksums
          --if realcrc == expectedcrc then
          if True then
              return $ Right scprec
            else
              return $ Left $ "expected csum:" ++ (show expectedcrc)
                              ++ " real csum:" ++ (show realcrc)
        Left err -> return $ Left err
    else
      return $ Left $ "expected size:" ++ (show expectedsize)
                      ++ " real size:" ++ (show realsize)

main = do
  args <- getArgs
  hdls <- case args of
    [] -> hSetBinaryMode stdin True >> return [stdin]
    otherwise -> mapM (\fn -> openBinaryFile fn ReadMode) args
  conts <- mapM size_n_contents hdls
  scprecs <- mapM (\(size, cont)
               -> return $ runGet (parseSCPfile size) cont) conts
  print scprecs
  where
    size_n_contents fh = do
      asksize <- hIsSeekable fh
      maybesize <- if asksize then sequence (Just (hFileSize fh))
                              else return Nothing
      contents <- hGetContents fh
      return (maybesize, contents)
