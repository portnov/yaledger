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
  let argv = words "-C test.accounts -M test.map -f -fbenchmark.yaledger -e 2030/01/01 turnovers -t"
  options <- parseCmdLine argv
  case options of
    Help -> return ()
    _ -> do
         let report = head $ reportParams options
             params = tail $ reportParams options
             inputPaths = if null (files options)
                            then [dataDir </> "default.yaledger"]
                            else files options
         case lookupInit report reports of
           [] -> putStrLn $ "No such report: " ++ report ++
                            "\nSupported reports are: " ++
                            unwords (map fst reports)
           [fn] -> do
               setupLogger (logSeverity options)
               C.defaultMain [
                 C.bench "run" $ C.whnfIO ( do
                   runYaLedger (chartOfAccounts options)
                       (accountMap options)
                       (parserConfigs options)
                       (reportsInterval options)
                       (query options)
                       (deduplicationRules options)
                       (files options) fn params ) ]
           _ -> putStrLn $ "Ambigous report specification: " ++ report ++
                           "\nSupported reports are: " ++
                           unwords (map fst reports)

