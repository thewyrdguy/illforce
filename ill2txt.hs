module Main where

import System.Environment (getArgs)
import Text.Printf (printf)
import Data.List (find)
import Data.Word (Word16)
import Data.Bits ((.&.), testBit)
import SCPECG
import SCPECG.Signal (SCPSignal(..))

main = do
  args <- getArgs
  record <- parseSCPFiles args
  case find getS6 record of
    Just (Right (S6 signal)) ->
      mapM_ (putStrLn . format) $ fmap (repr (scpSignaluSper1 signal)
                                             (scpSignalnVper1 signal))
                                        (zip [0 ..] (scpSignalData signal))
    _ -> putStrLn "Section 6 is absent"
  where
    getS6 (Right (S6 _)) = True
    getS6 _              = False
    repr :: Word16 -> Word16 -> (Int, Word16) -> (Float, Float, Int, Int)
    repr period multiplier (sample, value) =
      ( (fromIntegral period) * (fromIntegral sample) * 0.000001
      , (fromIntegral multiplier) * (fromIntegral (value .&. 0x0fff)) * 0.000001
      , if testBit value 15 then 1 else 0
      , if testBit value 14 then 1 else 0
      )
    format (sec, volt, isqrs, isanomaly) =
      printf "%9.3f %07.5f %1d %1d" sec volt isqrs isanomaly
