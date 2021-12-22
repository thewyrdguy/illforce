module SCPECG.Types (SCPSection, parseSection) where

import Data.Binary.Get (Get)
import Data.Word (Word16)

class SCPSection a where
  -- | Parser (Data.Binary.Get) for a section of SCP
  parseSection :: Integer                -- ^ Section number / code
               -> Word16                 -- ^ Length of data to consume
               -> Get (Either String a)  -- ^ returns error string or object
