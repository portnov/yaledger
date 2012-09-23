{-# LANGUAGE OverlappingInstances #-}
module Main where

import YaLedger.Types.Reports
import YaLedger.Main
import YaLedger.Reports.Cat
import YaLedger.Reports.Plan
import YaLedger.Reports.Balance
import YaLedger.Reports.Details
import YaLedger.Reports.Registry
import YaLedger.Reports.IncomeStatement

main :: IO ()
main =
  defaultMain [("balance",  Report balance),
               ("registry", Report registry),
               ("details",  Report details),
               ("plan",     Report showPlan),
               ("cat",      Report cat),
               ("incomestatement", Report incomeStatement)]
