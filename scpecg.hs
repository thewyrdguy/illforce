import System.Environment (getArgs)
import SCPECG

main = do
  args <- getArgs
  parseSCPFiles args >>= print
