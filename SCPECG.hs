module SCPECG (parseSCPFiles, SCPRec(..), SCPSec(..)) where

import Control.Exception (try)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT, withExceptT)
import Control.Monad.Trans.Class (lift)
import System.IO (IOMode(ReadMode), openBinaryFile, hSetBinaryMode
                 ,hIsSeekable, hFileSize, stdin)
import Data.ByteString.Lazy (hGetContents)
import SCPECG.Core

parseSCPFiles :: [String] -> IO (Either IOError SCPRec)
parseSCPFiles fnames = runExceptT $ do
  hdls <- ExceptT $ case fnames of
    [] -> hSetBinaryMode stdin True >> return (Right [stdin])
    otherwise -> try $ mapM (\fn -> openBinaryFile fn ReadMode) fnames
  conts <- lift $ mapM size_n_contents hdls
  let sections = map (uncurry parseSCP) conts
  withExceptT userError $ ExceptT $ return $ mergeSCP sections
  where
    size_n_contents fh = do
      asksize <- hIsSeekable fh
      maybesize <- if asksize then hFileSize fh >>= return . Just
                              else return Nothing
      contents <- hGetContents fh
      return (maybesize, contents)
