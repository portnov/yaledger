{-# LANGUAGE OverlappingInstances #-}
module Main where

import YaLedger.Types.Reports
import YaLedger.Main
import YaLedger.Reports.Cat
import YaLedger.Reports.Holds
import YaLedger.Reports.CoA
import YaLedger.Reports.Balance
import YaLedger.Reports.Saldo
import YaLedger.Reports.Details
import YaLedger.Reports.Registry
import YaLedger.Reports.Postings
import YaLedger.Reports.Turnovers
import YaLedger.Reports.IncomeStatement
import YaLedger.Reports.Stats
import YaLedger.Reports.Flow
import YaLedger.Reports.Rates

main :: IO ()
main =
  defaultMain
    [("balances", Report Balances),
     ("saldo",    Report Saldo),
     ("registry", Report Registry),
     ("postings", Report Postings),
     ("details",  Report Details),
     ("turnovers", Report Turnovers),
     ("accounts", Report CoA),
     ("cat",      Report Cat),
     ("holds",    Report Holds),
     ("incomestatement", Report IncomeStatement),
     ("stats",    Report Stats),
     ("rates",    Report RatesReport),
     ("flow",     Report Flow)]
