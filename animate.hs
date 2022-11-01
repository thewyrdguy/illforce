module Main where

-- to run inside ghci:
-- LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libglut.so.3 ghci ...

import System.Console.GetOpt
import System.Environment
import System.IO
import System.FilePath
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.List (find)
import Data.IORef
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate

data Options = Options { optVerbose :: Bool
                       , optFullscr :: Bool
                       , optBegin   :: Maybe Float
                       , optEnd     :: Maybe Float
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

data FinishException = Finish deriving Show
instance Exception FinishException

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
  let beg = fromMaybe 0.0 (optBegin opts)
  let end = optEnd opts
  datap <- newIORef [( (fromIntegral i) * (-0.006667) + beg :: Float
                       , 1.65 :: Float
                       , 0 :: Int
                       , 0 :: Int
                       ) | i <- [1..4000]]
  consume beg fh
  -- For whatever reason, catching exception does not work.
  catch (animateFixedIO screenmode black (step beg end datap fh) ctrl)
        (\e -> putStrLn $ "caught exception" ++ (show (e :: FinishException)))
  putStrLn $ "After animation"

consume :: Float -> Handle -> IO ()
consume beg fh = do
  (x, _, _, _) <- fmap parseline $ hGetLine fh
  when (x < beg) $ consume beg fh
  return ()

step :: Float -> Maybe Float -> IORef [(Float, Float, Int, Int)] -> Handle -> Float -> IO Picture
step beg end datap fh tm = do
  when (fromMaybe False (fmap (< tm) end)) $ throwIO Finish
  indata <- readIORef datap
  newdata <- advance indata fh (tm + beg)
  writeIORef datap newdata
  return $ picture (beg + tm) newdata

advance :: [(Float, Float, Int, Int)]
        -> Handle
        -> Float
        -> IO [(Float, Float, Int, Int)]
advance indata fh atm
  | atm < (let (x, _, _, _) = (head indata) in x) = return indata
  | otherwise = do
    new <- fmap parseline $ hGetLine fh
    advance (take 4000 (new:indata)) fh atm

parseline = (\[s0, s1, s2, s3] ->
                   ( read s0 :: Float
                   , read s1 :: Float
                   , read s2 :: Int
                   , read s3 :: Int)) . words

grid = color (greyN 0.2) (pictures
         [line [(x * 256.0, -360.0), (x * 256.0, 360.0)]
          | x <- [-2.5,-2.46..2.5]])
    <> color (greyN 0.2) (pictures
         [line [(-640.0, y * 115.0),(640.0, y * 115.0)]
          | y <- [-3.0,-2.90..3.0]])
    <> color (greyN 0.5) (pictures
         [line [(x * 256, -360.0),(x * 256, 360.0)]
          | x <- [-2.5,-2.3..2.5]])
    <> color (greyN 0.5) (pictures
         [line [(-640.0, y * 115.0),(640.0, y * 115.0)]
          | y <- [-3.0,-2.5..3.0]])


picture tm indata = translate off 0 grid <> curve
  where
    off = tm - fromIntegral (round (tm * 5.0)) / 5.0
    curve = color (bright green) $ line (map fit indata)
    fit :: (Float, Float, Int, Int) -> (Float, Float)
    fit (ts, mV, qrs, ano) =
      ((ts - tm) * (1280.0) / 5.0 + 640, (mV - 1.65) * 450.0)
      

ctrl :: Controller -> IO ()
ctrl req = putStrLn "ctrl callback"
