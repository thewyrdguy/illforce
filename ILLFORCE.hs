module ILLFORCE (listIllForceDir) where

import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, throwE)
--import Control.Monad.Trans.Class (lift)
--import Control.Monad.IO.Class (liftIO)
import Control.Exception (try)
import System.IO (readFile)
import System.IO.Error (IOError, mkIOError, userError)
import System.FilePath (FilePath, (</>))
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, eof, satisfy,
                                     char, string, munch1,
                                     skipSpaces, many1, skipMany)
import Data.Char (isDigit, isHexDigit)
import Text.Read.Lex (readDecP, readHexP)

listIllForceDir :: FilePath -> IO (Either IOError [FilePath])
listIllForceDir path = runExceptT $ do
-- run :: FilePath -> ExceptT IOError IO [FilePath]
  readme <- ExceptT $ try $ readFile (path </> "README.TXT")
  ExceptT $ return $ pr2either $ readP_to_S parseidx readme
  where
    pr2either [] = Left $ userError "Failed to parse README.TXT"
    pr2either [(x, "")] = Right x
    pr2either r = Left $ userError $ "Inconclusive parse of README.TXT: "
                                ++ (show r)

eol = do
  skipMany (char ' ')
  _ <- munch1 ((\x -> (x == '\n') || (x == '\r')))
  return ()

parseidx = do
  string "Easy ECG Monitor" >> eol
  string "Max SCP File Number in Dir:" >> eol
  f_per_d <- readDecP
  eol
  string "Version ID:" >> eol
  ver_id <- munch1 (\x -> isDigit x || x == '.')
  eol
  string "Product ID:" >> eol
  prod_id <- munch1 isHexDigit
  eol
  string "Total records:" >> skipMany (char ' ')
  total <- munch1 isDigit
  eol
  string "No." >> skipSpaces >> string "From"
  skipSpaces >> string "To" >> eol
  munch1 (const True)  -- remove this
  eof
  return [(show f_per_d), ver_id, prod_id, total]
