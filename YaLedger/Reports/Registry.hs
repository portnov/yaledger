{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, RecordWildCards, TypeFamilies #-}

module YaLedger.Reports.Registry
  (Registry (..)) where

import YaLedger.Reports.API

data Registry = Registry

type ROptions = CommonFlags

instance ReportClass Registry where
  type Options Registry = ROptions
  type Parameters Registry = Maybe Path

  reportHelp _ = "Show all entries in account or group of accounts."

  reportOptions _ =
    [Option "l" ["ledger"] (NoArg CLedgerBalances) "Show ledger balances instead of available balances",
     Option "b" ["both"] (NoArg CBothBalances) "Show both available and ledger balances",
     Option "a" ["absolute"] (NoArg CAbsoluteValues) "Show absolute values of all balances",
     Option ""  ["no-currencies"] (NoArg CNoCurrencies) "Do not show currencies in amounts",
     Option "C" ["csv"] (OptArg CCSV "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)"]

  runReport _ qry options mbPath = 
      registry qry options mbPath
    `catchWithSrcLoc`
      (\l (e :: InternalError) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: InvalidPath) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: NoSuchRate) -> handler l e)

absBalance extBalance@(Ext {getContent = balance}) =
  extBalance {
    getContent = balance {
                   balanceValue = abs (balanceValue balance)
                 }
             }

registry qry options mbPath = do
    fullCoA <- gets lsCoA
    coa <- case mbPath of
              Nothing   -> return fullCoA
              Just path -> getCoAItem (gets lsPosition) (gets lsCoA) path
    totals <- do
              res <- treeSaldo qry coa
              case res of
                Leaf {..}   -> return leafData
                Branch {..} -> return branchData
    let showCurrs = CNoCurrencies `notElem` options
    case coa of
      Leaf {leafData = account} -> do
          balances <- readIOListL (accountBalances account)
          let mbAbs = if CAbsoluteValues `elem` options
                        then map absBalance
                        else id
          let balances' = mbAbs $ reverse $ filter (checkQuery qry) balances
          let bqry = if CBothBalances `elem` options
                        then BothBalances
                        else if CLedgerBalances `elem` options
                              then Only LedgerBalance
                              else Only AvailableBalance
          let format = case [s | CCSV s <- options] of
                         []    -> showEntriesBalances' bqry showCurrs fullCoA ASCII totals
                         (x:_) -> showEntriesBalances' bqry showCurrs fullCoA (CSV x) totals
          wrapIO $ putStr $ format (nub $ balances')
      Branch {} -> do
          let accounts = map snd $ leafs coa
          allEntries <- forM accounts getEntries
          let entries = reverse $ concat $ map (filter $ checkQuery qry) allEntries
          let format = case [s | CCSV s <- options] of
                         []    -> showEntries' fullCoA ASCII totals showCurrs
                         (x:_) -> showEntries' fullCoA (CSV x) totals showCurrs
          wrapIO $ putStr $ format (nub $ sort $ entries)
  
