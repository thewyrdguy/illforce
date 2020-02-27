module ILLFORCE (listIllForceDir) where

import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, throwE)
import Control.Exception (try)
import System.IO (readFile)
import System.IO.Error (IOError, userError)
import System.FilePath (FilePath, (</>))
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, eof, satisfy,
                                     char, string, munch1, skipSpaces,
                                     many, many1, skipMany)
import Text.Read.Lex (readDecP, readHexP)
import Data.Char (isDigit, isHexDigit)

listIllForceDir :: FilePath -> IO (Either IOError [(Int, [FilePath])])
listIllForceDir path = runExceptT $ do
  readme <- ExceptT $ try $ readFile (path </> "README.TXT")
  let
    readp2either [(x, "")] = Right x
    readp2either [] = Left $ userError $ "Failed to parse README.TXT"
    readp2either r  = Left $ userError $ "Inconclusive parse of README.TXT: "
                                         ++ (show r)
  (f_per_d, ver_id, prod_id, total, groups)
    <- ExceptT $ return $ readp2either $ readP_to_S parsereadme readme
  when (total /= length groups) $ throwE $ userError $ "Wrong record number"
  let
 -- base [] = 0  -- Can never be called with empty list
    base ((_, fm, _):xs) = fm
    subidx n = (n - base groups) `div` f_per_d
    mkpathlist (no, fm, to) = (no, map mkpath [fm .. to])
    mkpath n = path </> "ECG_" ++ show (subidx n) </> show n ++ ".SCP"
  return $ map mkpathlist groups

parsereadme = do
  let
    eol = do
      skipMany (char ' ')
      munch1 ((\x -> (x == '\n') || (x == '\r')))
      return ()
  string "Easy ECG Monitor"; eol
  string "Max SCP File Number in Dir:"; eol
  f_per_d <- readDecP; eol
  string "Version ID:"; eol
  ver_id <- many1 (satisfy (\x -> isDigit x || x == '.')); eol
  string "Product ID:"; eol
  prod_id <- many1 (satisfy isHexDigit); eol
  string "Total records:"; skipMany (char ' ')
  total <- readDecP; eol
  string "No."; skipSpaces; string "From"; skipSpaces; string "To"; eol
  groups <- many $ do
    no <- readDecP; skipSpaces
    fm <- readDecP; string ".scp"; skipSpaces
    to <- readDecP; string ".scp"; eol
    return (no, fm, to)
  skipSpaces; eof
  return (f_per_d, ver_id, prod_id, total, groups)
