module SCPECG (parseSCPFiles, SCPRec) where

import System.IO (IOMode(ReadMode), openBinaryFile, hSetBinaryMode
                 ,hIsSeekable, hFileSize, stdin)
import Data.ByteString.Lazy (hGetContents)
import SCPECG.Core

parseSCPFiles :: [String] -> IO (Either String SCPRec)
parseSCPFiles fnames = do
  hdls <- case fnames of
    [] -> hSetBinaryMode stdin True >> return [stdin]
    otherwise -> mapM (\fn -> openBinaryFile fn ReadMode) fnames
  conts <- mapM size_n_contents hdls
  let sections = map (uncurry parseSCP) conts
  return $ mergeSCP sections
  where
    size_n_contents fh = do
      asksize <- hIsSeekable fh
      maybesize <- if asksize then hFileSize fh >>= return . Just
                              else return Nothing
      contents <- hGetContents fh
      return (maybesize, contents)
