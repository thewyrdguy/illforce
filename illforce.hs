module Main where

import System.Exit (exitWith, ExitCode(..))
import System.Environment (getArgs, getProgName)
import System.IO (writeFile)
import System.FilePath (FilePath, (</>))
import System.Console.GetOpt (getOpt, ArgOrder(Permute), usageInfo,
                              OptDescr(Option), ArgDescr(..))
import Text.Printf (printf)
import Data.List (find)
import Data.Word (Word16)
import Data.Bits ((.&.), testBit)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except ( except, ExceptT(ExceptT), runExceptT
                                  , withExceptT)
import Control.Exception (try)
import SCPECG
import SCPECG.Signal (SCPSignal(..))
import ILLFORCE

data Operation = OpUnspec
               | OpList
               | OpText
               deriving (Eq, Show)

data Options = Options { optVerbose :: Bool
                       , optOp      :: Operation
                       , optPath    :: FilePath
                       , optOut     :: FilePath
                       } deriving Show

defaultOptions = Options { optVerbose = False
                         , optOp      = OpUnspec
                         , optPath    = undefined
                         , optOut     = "."
                         }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["verbose"]
      (NoArg (\opts -> opts { optVerbose = True }))
      "Verbose output"
  , Option ['L'] ["list"]
      (NoArg (\opts -> opts { optOp = OpList }))
      "List Records"
  , Option ['T'] ["text"]
      (NoArg (\opts -> opts { optOp = OpText }))
      "Translate records to text format"
  , Option ['O'] ["out"]
      (ReqArg (\d opts -> opts { optOut = d }) "DIR")
      "Output directory"
  ]

parseargs progname argv =
  case getOpt Permute options argv of
    (o,[p],[]  ) ->
      let
        opts = (foldl (flip id) defaultOptions o) {optPath = p}
      in
        if (optOp opts) /= OpUnspec
          then Right opts
          else Left $ userError ("Operation not specified\n"
                                 ++ usageInfo header options)
    (_, _ ,[]  ) -> Left $ userError ("Source dir not specified\n"
                                      ++ usageInfo header options)
    (_, _ ,errs) -> Left $ userError (concat errs ++ usageInfo header options)
  where header = "Usage: " ++ progname ++ " [OPTION...] source-path"

main =
  (runExceptT $ do
    progname <- lift getProgName
    args <- lift getArgs
    opts <- ExceptT $ return $ parseargs progname args
    drec <- ExceptT $ listIllForceDir (optPath opts)
    case optOp opts of
      OpList -> listrecs opts drec
      OpText -> outtexts opts drec
  ) >>= either (\err -> print err >> exitWith (ExitFailure 1))
               (\_   -> return ())

listrecs opts dirrec = do
  let heads = map (\(n, xs) -> (n, head xs)) dirrec
  lift $ mapM (putStrLn . show) heads

outtexts opts dirrec = mapM (outtext opts) dirrec
  where
  outtext opts (n, paths) = do
    let outname = (optOut opts) </> (show n) ++ ".txt"
    record <- ExceptT $ parseSCPFiles paths
    signal <- except $
      fromMaybe (Left (userError "Signal section not found"))
                (fmap Right (s6 record))
    ExceptT $ try $ writeFile outname (unlines (formattext signal))

  formattext signal = map formatentry (normalize signal)
  normalize signal = map (normentry (scpSignaluSper1 signal)
                                    (scpSignalnVper1 signal))
                            (zip [0 ..] (scpSignalData signal))
  normentry :: Word16 -> Word16 -> (Int, Word16) -> (Float, Float, Int, Int)
  normentry period multiplier (sample, value) =
    ( (fromIntegral period) * (fromIntegral sample) * 0.000001
    , (fromIntegral multiplier) * (fromIntegral (value .&. 0x0fff)) * 0.000001
    , if testBit value 15 then 1 else 0
    , if testBit value 14 then 1 else 0
    )
  formatentry :: (Float, Float, Int, Int) -> String
  formatentry (sec, volt, isqrs, isanomaly) =
    printf "%9.3f %07.5f %1d %1d" sec volt isqrs isanomaly
