module Main where

import System.Console.GetOpt
import System.Environment
import System.IO
import System.FilePath
import Data.List (find)
import Data.IORef
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate

data Options = Options { optVerbose :: Bool
                       , optFullscr :: Bool
                       , optBegin   :: Maybe Int
                       , optEnd     :: Maybe Int
                       , optPath    :: Maybe FilePath
                       } deriving Show
defaultOptions = Options { optVerbose = False
                         , optFullscr = False
                         , optBegin   = Nothing
                         , optEnd     = Nothing
                         , optPath    = Nothing
                         }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["verbose"]
      (NoArg (\opts -> opts { optVerbose = True }))
      "Verbose output"
  , Option ['f'] ["fullscreen"]
      (NoArg (\opts -> opts { optFullscr = True }))
      "Verbose output"
  , Option ['b'] ["begin"]
      (ReqArg (\x opts -> opts { optBegin = Just (read x) }) "beg")
      "Start second (default from the start of file)"
  , Option ['e'] ["end"]
      (ReqArg (\x opts -> opts { optEnd = Just (read x) }) "end")
      "End second (default till the end of file)"
  ]

parseargs progname argv =
  case getOpt Permute options argv of
    (o,[p],[]  ) -> return (foldl (flip id) defaultOptions o) {optPath = Just p}
    (o,[],[]  ) -> return (foldl (flip id) defaultOptions o) {optPath = Nothing}
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: " ++ progname ++ " [OPTION...] source-path"

main = do
  progname <- getProgName
  args <- getArgs
  opts <- parseargs progname args
  let
    screenmode = if optFullscr opts
                   then FullScreen
                   else InWindow "Signal" (1280, 720) (500, 500)
  fh <- case optPath opts of
          Just p  -> openFile p ReadMode
          Nothing -> return stdin
  datap <- newIORef $ replicate 4000
                      (0.0 :: Float, 1.5 :: Float, 0 :: Int, 0 :: Int)
  animateFixedIO screenmode black (step datap fh) ctrl

step :: IORef [(Float, Float, Int, Int)] -> Handle -> Float -> IO Picture
step datap fh tm = do
  indata <- readIORef datap
  newdata <- advance indata fh tm
  writeIORef datap newdata
  return $ picture newdata

advance :: [(Float, Float, Int, Int)]
        -> Handle
        -> Float
        -> IO [(Float, Float, Int, Int)]
advance indata fh atm
  | atm < (let (x, _, _, _) = (head indata) in x) = return indata
  | otherwise = do
    new <- fmap ((\[s0, s1, s2, s3] ->
                   ( read s0 :: Float
                   , read s1 :: Float
                   , read s2 :: Int
                   , read s3 :: Int)) . words) $ hGetLine fh
    advance (take 4000 (new:indata)) fh atm

picture indata =
  Color green $ translate (600) (-350) $ line (map fit (zip [0..] indata))
  where
    fit :: (Int, (Float, Float, Int, Int)) -> (Float, Float)
    fit (num, (ts, mV, qrs, ano)) =
      ((fromIntegral num) * (-10280.0) / 4000.0, mV * 720.0 / 3.0)
      

ctrl :: Controller -> IO ()
ctrl req = putStrLn "ctrl callback"
