module Main where

import System.Exit (exitWith, ExitCode(..))
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt (getOpt, ArgOrder(Permute), usageInfo,
                              OptDescr(Option), ArgDescr(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import SCPECG
import SCPECG.Signal (SCPSignal(..))
import ILLFORCE

data Operation = OpUnspec | OpList deriving (Eq, Show)

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
  ) >>= either (\err -> print err >> exitWith (ExitFailure 1))
               (\_   -> return ())

listrecs opts dirrec = do
  let heads = map (\(n, xs) -> (n, head xs)) dirrec
  lift $ mapM (putStrLn . show) heads
