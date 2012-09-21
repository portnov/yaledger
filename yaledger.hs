{-# LANGUAGE OverlappingInstances #-}
module Main where

import YaLedger.Types.Reports
import YaLedger.Main
import YaLedger.Reports.Balance
import YaLedger.Reports.Registry

main :: IO ()
main =
  defaultMain [("balance",  Report balance),
               ("registry", Report registry)]
