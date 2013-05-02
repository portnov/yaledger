
module YaLedger.Types.Output
  (ASCII (..),
   CSV (..),
   OutputFormat (..),
   defaultOutputFormat
  ) where

import YaLedger.Output.ASCII
import YaLedger.Output.CSV

data OutputFormat =
       OASCII ASCII
     | OCSV CSV
  deriving (Eq, Show)

defaultOutputFormat :: OutputFormat
defaultOutputFormat = OASCII ASCII

