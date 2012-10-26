{-# LANGUAGE OverlappingInstances #-}
module Main where

import Control.Monad
import System.Environment.XDG.BaseDir
import System.FilePath
import qualified Criterion.Main as C
import Data.Dates

import YaLedger.Logger
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

reports = [("balances",  Report Balances),
                 ("registry", Report Registry),
                 ("postings", Report Postings),
                 ("details",  Report Details),
                 ("turnovers", Report Turnovers),
                 ("accounts", Report CoA),
                 ("cat",      Report Cat),
                 ("incomestatement", Report IncomeStatement)]

main :: IO ()
main = do
  dataDir <- getUserDataDir "yaledger"
  let argv = words "-r ./examples/currencies.yaml -C examples/default.accounts -M examples/test.map -f -fexamples/benchmark.yaledger -e 2030/01/01 turnovers -t"
  init <- initialize allParsers reports argv
  case init of
    Nothing -> return ()
    Just (report, options, params) ->
         C.defaultMain [
           C.bench "run" $ C.whnfIO ( runYaLedger allParsers options report params )
           ]

