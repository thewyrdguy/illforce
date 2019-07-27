module SCPECG.Types (SCPSection, parseSection) where

import Data.Binary.Get (Get)
import Data.Word (Word16)

class SCPSection a where
  parseSection :: Integer -> Word16 -> Get (Either String a)
