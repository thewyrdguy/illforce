module Main where

-- to run inside ghci:
-- LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libglut.so.3 ghci ...

import Control.Exception
import Control.Monad
import Data.Maybe
import Data.List (find)
import Data.List.Split (splitOn)
import Data.IORef
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import System.Console.GetOpt
import System.Environment
import System.IO
import System.FilePath
import Text.Printf

data Options = Options { optVerbose :: Bool
                       , optFullscr :: Bool
                       , optCrt     :: Bool
                       , optBegin   :: Maybe Float
                       , optEnd     :: Maybe Float
                       , optPath    :: Maybe FilePath
                       , optRes     :: String
                       } deriving Show
defaultOptions = Options { optVerbose = False
                         , optFullscr = False
                         , optCrt     = False
                         , optBegin   = Nothing
                         , optEnd     = Nothing
                         , optPath    = Nothing
                         , optRes     = "1280x720"
                         }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["verbose"]
      (NoArg (\opts -> opts { optVerbose = True }))
      "Verbose output"
  , Option ['f'] ["fullscreen"]
      (NoArg (\opts -> opts { optFullscr = True }))
      "Verbose output"
  , Option ['c'] ["crt"]
      (NoArg (\opts -> opts { optCrt = True }))
      "CRT emulation mode (otherwise running tape emulation)"
  , Option ['b'] ["begin"]
      (ReqArg (\x opts -> opts { optBegin = Just (read x) }) "beg")
      "Start second (default from the start of file)"
  , Option ['e'] ["end"]
      (ReqArg (\x opts -> opts { optEnd = Just (read x) }) "end")
      "End second (default till the end of file)"
  , Option ['r'] ["resolution"]
      (ReqArg (\x opts -> opts { optRes = x }) "resolution")
      "Resolution of the rendered video NNNxMMM (xres 'x' yres)"
  ]

parseargs progname argv =
  case getOpt Permute options argv of
    (o,[p],[]  ) -> return (foldl (flip id) defaultOptions o) {optPath = Just p}
    (o,[],[]  ) -> return (foldl (flip id) defaultOptions o) {optPath = Nothing}
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: " ++ progname ++ " [OPTION...] input-file-path"

data FinishException = Finish deriving Show
instance Exception FinishException

main = do
  progname <- getProgName
  args <- getArgs
  opts <- parseargs progname args
  let
    [xres :: Int, yres :: Int] = map read $ splitOn "x" (optRes opts)
    screenmode = if optFullscr opts
                   then FullScreen
                   else InWindow "Signal" (xres, yres) (500, 500)
    beg = fromMaybe 0.0 (optBegin opts)
    end = optEnd opts
    picfun = if optCrt opts then crtPic else streamPic
  fh <- case optPath opts of
          Just p  -> openFile p ReadMode
          Nothing -> return stdin
  datap <- newIORef [( (fromIntegral i) * (-0.006667) + beg :: Float
                       , 1.65 :: Float
                       , 0 :: Int
                       , 0 :: Int
                       ) | i <- [1..4000]]
  consume beg fh
  -- For whatever reason, catching exception does not work.
  catch (animateFixedIO screenmode black (step picfun beg end datap fh) ctrl)
        (\e -> putStrLn $ "caught exception" ++ (show (e :: FinishException)))
  putStrLn $ "After animation"

consume :: Float -> Handle -> IO ()
consume beg fh = do
  (x, _, _, _) <- fmap parseline $ hGetLine fh
  when (x < beg) $ consume beg fh
  return ()

step :: (Float -> [(Float, Float, Int, Int)] -> Picture)
     -> Float
     -> Maybe Float
     -> IORef [(Float, Float, Int, Int)]
     -> Handle
     -> Float
     -> IO Picture
step picfun beg end datap fh tm = do
  when (fromMaybe False (fmap (< tm) end)) $ throwIO Finish
  indata <- readIORef datap
  newdata <- advance indata fh (tm + beg)
  writeIORef datap newdata
  -- print (tm, head newdata, last newdata, length (takeWhile (\(t, _, _, _) -> t > 0) newdata))
  return $ picfun (beg + tm) newdata

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
          | x <- [-2.5,-2.46..2.7]])
    <> color (greyN 0.2) (pictures
         [line [(-640.0, y * 115.0), (690.0, y * 115.0)]
          | y <- [-3.0,-2.90..3.0]])
    <> color (greyN 0.5) (pictures
         [line [(x * 256, -360.0),  (x * 256, 360.0)]
          | x <- [-2.5,-2.3..2.7]])
    <> color (greyN 0.5) (pictures
         [line [(-640.0, y * 115.0), (690.0, y * 115.0)]
          | y <- [-3.0,-2.5..3.0]])


label x y str = translate x y $ scale 0.1 0.1 $ text str

fit :: Float -> (Float, Float, Int, Int) -> (Float, Float)
fit tm (ts, mV, qrs, ano) =
  ((ts - tm) * 256.0 + 640, (mV - 1.65) * 450.0)

streamPic tm indata = translate (off * (-256)) 0 (grid <> xlabels (tm - 2.5))
                   <> curve
  where
    mark = fromIntegral (floor (tm * 5.0)) / 5.0
    off = tm - mark
    curve = color (bright green) $ line (map (fit tm) indata)
    xlabels tm = color white (pictures
             [label (x * 256) (-355) (printf "%4.1f" (x + mark - 2.5))
              | x <- [-2.5,-2.0..2.5]])


crtPic tm indata = grid <> xlabels base <> curve
  where
    base = fromIntegral (floor (tm / 5.0)) * 5.0
    -- cutoff = tm - base
    (newdata, rest) = span (\(t, _, _, _) -> t > base) indata
    olddata = takeWhile (\(t, _, _, _) -> t > (tm - 4.8)) rest
    curve = color (bright green) (line (map (fit (5.0 + base)) newdata))
         <> color (dim green) (line (map (fit (0.0 + base)) olddata))
    xlabels tm = color white (pictures
             [label (x * 256) (-355) (printf "%4.1f" (x + base + 2.5))
              | x <- [-2.5,-2.0..2.5]])

ctrl :: Controller -> IO ()
ctrl req = putStrLn "ctrl callback"
