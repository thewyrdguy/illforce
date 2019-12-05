import System.Environment
import Data.List (find)
import Data.IORef
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate

main = do
  args <- getArgs
  let
    screenmode = case find (== "-f") args of
      Just _  -> FullScreen
      Nothing -> InWindow "Signal" (1280, 720) (500, 500)
  datap <- newIORef $ replicate 4000
                      (0.0 :: Float, 1.5 :: Float, 0 :: Int, 0 :: Int)
  animateFixedIO screenmode white (step datap) ctrl

step :: IORef [(Float, Float, Int, Int)] -> Float -> IO Picture
step datap tm = do
  indata <- readIORef datap
  newdata <- advance indata tm
  writeIORef datap newdata
  return $ picture newdata

advance :: [(Float, Float, Int, Int)]
        -> Float
        -> IO [(Float, Float, Int, Int)]
advance indata atm
  | atm < (let (x, _, _, _) = (head indata) in x) = return indata
  | otherwise = do
    new <- fmap ((\[s0, s1, s2, s3] ->
                   ( read s0 :: Float
                   , read s1 :: Float
                   , read s2 :: Int
                   , read s3 :: Int)) . words) $ getLine
    advance (take 4000 (new:indata)) atm

picture indata =
  translate (600) (-350) $ line (map fit (zip [0..] indata))
  where
    fit :: (Int, (Float, Float, Int, Int)) -> (Float, Float)
    fit (num, (ts, mV, qrs, ano)) =
      ((fromIntegral num) * (-10280.0) / 4000.0, mV * 720.0 / 3.0)
      

ctrl :: Controller -> IO ()
ctrl req = putStrLn "ctrl callback"
