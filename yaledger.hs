{-# LANGUAGE OverlappingInstances #-}
module Main where

import YaLedger.Types.Reports
import YaLedger.Main
import YaLedger.Reports.Cat
import YaLedger.Reports.CoA
import YaLedger.Reports.Balance
import YaLedger.Reports.Details
import YaLedger.Reports.Registry
import YaLedger.Reports.Postings
import YaLedger.Reports.IncomeStatement

main :: IO ()
main =
  defaultMain [("balance",  Report balance),
               ("registry", Report registry),
               ("postings", Report postings),
               ("details",  Report details),
               ("accounts", Report showCoA),
               ("cat",      Report cat),
               ("incomestatement", Report incomeStatement)]
