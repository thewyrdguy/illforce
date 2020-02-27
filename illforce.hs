module Main where

import System.Console.GetOpt (getOpt, ArgOrder(Permute), usageInfo,
                              OptDescr(Option), ArgDescr(..))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import System.Environment (getArgs)
import Text.Printf (printf)
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

parseargs argv =
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
  where header = "Usage: [OPTION...] source-path"

main = do
  result <- runExceptT go
  case result of
    Left err -> print err
    Right _  -> return ()
  where
    go = do
      args <- ExceptT $ fmap Right getArgs
      opts <- ExceptT $ return $ parseargs args
      dirrec <- ExceptT $ listIllForceDir (optPath opts)
      let heads = map (\(n, xs) -> (n, head xs)) dirrec
      ExceptT $ fmap Right $ mapM (putStrLn . show) heads
      return ()
