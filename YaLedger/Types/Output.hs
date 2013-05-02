{-# LANGUAGE Rank2Types #-}
module YaLedger.Types.Output
  (ASCII (..),
   CSV (..),
   OutputFormat (..),
   defaultOutputFormat
  ) where

import YaLedger.Output.Tables
import YaLedger.Output.ASCII
import YaLedger.Output.CSV
import YaLedger.Output.HTML

data OutputFormat =
       OASCII ASCII
     | OCSV CSV
     | OHTML HTML
  deriving (Eq, Show)

defaultOutputFormat :: OutputFormat
defaultOutputFormat = OASCII ASCII

