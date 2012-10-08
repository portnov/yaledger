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
import YaLedger.Reports.Turnovers
import YaLedger.Reports.IncomeStatement

main :: IO ()
main =
  defaultMain
    allParsers
    [("balances",  Report Balances),
     ("registry", Report Registry),
     ("postings", Report Postings),
     ("details",  Report Details),
     ("turnovers", Report Turnovers),
     ("accounts", Report CoA),
     ("cat",      Report Cat),
     ("incomestatement", Report IncomeStatement)]
