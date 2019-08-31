import System.Environment (getArgs)
import Data.List (find)
import SCPECG
import SCPECG.Signal (SCPSignal(..))

main = do
  args <- getArgs
  record <- parseSCPFiles args
  case find getS6 record of
    Just (Right (S6 signal)) -> putStr $ unlines
                                       $ fmap format
                                       $ repr (scpSignaluSper1 signal)
                                              (scpSignalnVper1 signal)
                                              (scpSignalData signal)
    _                        -> print "absent"
  where
    getS6 (Right (S6 _)) = True
    getS6 _              = False
    repr period multiplier rawdata =
      zip [0.0, ((fromIntegral period) / 1000000.0) ..]
          (fmap ((* ((fromIntegral multiplier) * 0.001)) . fromIntegral)
                rawdata)
    format (t, v)
      | v < 0.0   = "" -- (show t) ++ " " ++ (show 0)
      | otherwise = (show t) ++ " " ++ (show v)
