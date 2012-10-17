{-# LANGUAGE ScopedTypeVariables #-}
module YaLedger.Tests.Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Data.Maybe
import Data.List
import qualified Data.Map as M

import YaLedger.Types
import YaLedger.Monad
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
    Just (qry, report, options, params) -> do
      currs <- loadCurrencies (fromJust $ currenciesList options)
      let currsMap = M.fromList [(cSymbol c, c) | c <- currs]
      coa <- readCoA currsMap (fromJust $ chartOfAccounts options)
      amap <- readAMap coa (fromJust $ accountMap options)
      records <- parseInputFiles parsers (parserConfigs options) currsMap coa (files options)
      accs <- loadAccs "correspondence.txt"
      let entries = [p | p@(Ext {getContent = Transaction (TEntry _)}) <- records]
      runLedger coa amap records $ runEMT $ do
        forM_ (zip entries accs) $ \(rec, list) -> do
          let cra = map fst list
              dta = map snd list
          let Transaction (TEntry entry) = getContent rec
          setPos (getLocation rec)
          testCheckEntry (getDate rec) (getAttributes rec) entry cra dta
            `catchWithSrcLoc`
              (\l (e :: NoCorrespondingAccountFound) -> handler l e)
            `catchWithSrcLoc`
              (\l (e :: NoSuchRate) -> handler l e)
            `catchWithSrcLoc`
              (\l (e :: InvalidAccountType) -> handler l e)
            `catchWithSrcLoc`
              (\l (e :: TestFailed) -> handler l e)
        wrapIO $ putStrLn "Correspondence test passed."

