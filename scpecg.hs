import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), openBinaryFile, hSetBinaryMode
                 ,hIsSeekable, hFileSize, stdin)
import Data.ByteString.Lazy (hGetContents)
import SCPECG.Core

main = do
  args <- getArgs
  hdls <- case args of
    [] -> hSetBinaryMode stdin True >> return [stdin]
    otherwise -> mapM (\fn -> openBinaryFile fn ReadMode) args
  conts <- mapM size_n_contents hdls
  let scprecs = map (\(size, cont) -> parseSCP size cont) conts
  print scprecs
  where
    size_n_contents fh = do
      asksize <- hIsSeekable fh
      maybesize <- if asksize then hFileSize fh >>= return . Just
                              else return Nothing
      contents <- hGetContents fh
      return (maybesize, contents)
