{-# LANGUAGE ScopedTypeVariables #-}
module YaLedger.Tests.Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Data.Maybe
import Data.List
import qualified Data.Map as M

import YaLedger.Types
import YaLedger.Kernel
import YaLedger.Main
import YaLedger.Exceptions
import YaLedger.Logger
import YaLedger.Parser
import YaLedger.Parser.Currencies
import YaLedger.Reports.Balance
import YaLedger.Tests.Instances
import YaLedger.Tests.Correspondence

main :: String -> IO ()
main args = do
  let argv = words args
      parsers = allParsers
      reports = [("balance", Report Balances)]
  init <- initialize parsers reports argv
  case init of
    Nothing -> return ()
    Just (report, options, params) -> do
      currs <- loadCurrencies (fromJust $ currenciesList options)
      let currsMap = M.fromList [(cSymbol c, c) | c <- currs]
      coa <- readCoA currsMap (fromJust $ chartOfAccounts options)
      amap <- readAMap coa (fromJust $ accountMap options)
      records <- parseInputFiles parsers (parserConfigs options) currsMap coa (files options)
      runTest "correspondence" $ correspondenceTest currs coa amap options records

