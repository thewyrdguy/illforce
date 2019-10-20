module ILLFORCE (listIllForceDir) where

import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, throwE)
--import Control.Monad.Trans.Class (lift)
--import Control.Monad.IO.Class (liftIO)
import Control.Exception (try)
import System.IO (readFile)
import System.IO.Error (IOError, mkIOError)
import System.FilePath (FilePath, (</>))
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import Text.ParserCombinators.ReadP

listIllForceDir :: FilePath -> IO (Either IOError [FilePath])
listIllForceDir path = runExceptT $ do
-- run :: FilePath -> ExceptT IOError IO [FilePath]
  illidx <- ExceptT $ try $ readFile (path </> "README.TXT")
  paths <- ExceptT $ return $ parseidx illidx
  return paths

parseidx :: String -> Either IOError [FilePath]
parseidx s = Right $ filter (any (\x -> x /= ' ' && x /= '\r')) (lines s)
