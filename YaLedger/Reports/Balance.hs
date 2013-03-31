{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies, RecordWildCards #-}

module YaLedger.Reports.Balance
  (Balances (..)) where

import YaLedger.Reports.API

data Balances = Balances

type BOptions = CommonFlags

instance ReportClass Balances where
  type Options Balances = BOptions
  type Parameters Balances = Maybe Path
  reportOptions _ = 
    [Option "z" ["no-zeros"] (NoArg CNoZeros) "Do not show accounts with zero balance",
     Option "p" ["positive"] (NoArg COnlyPositive) "Show only accounts with positive balance",
     Option "n" ["negative"] (NoArg COnlyNegative) "Show only accounts with negative balance",
     Option "a" ["absolute"] (NoArg CAbsoluteValues) "Show absolute values of all balances",
     Option "g" ["hide-groups"] (NoArg CHideGroups) "Hide accounts groups in CSV output",
     Option ""  ["no-currencies"] (NoArg CNoCurrencies) "Do not show currencies in amounts",
     Option "l" ["ledger"] (NoArg CLedgerBalances) "Show ledger balances instead of available balances",
     Option "b" ["both"] (NoArg CBothBalances) "Show both available and ledger balances",
     Option "C" ["csv"] (OptArg CCSV "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)"]
  defaultOptions _ = []
  reportHelp _ = "Show accounts balances. One optional parameter: account or accounts group."

  runReport _ qry opts mbPath = balance [qry] opts mbPath
  runReportL _ queries opts mbPath = balance queries opts mbPath

showI :: Query -> [String]
showI qry = [showD "now" (qEnd qry)]
  where
    showD s Nothing = s
    showD _ (Just date) = showDate date

selectBalance options bi
  | (CLedgerBalances `elem` options) || (CBothBalances `elem` options) = maybe 0 amountValue (biLedger bi)
  | otherwise = maybe 0 amountValue (biAvailable bi)

byBalance options bi
  | COnlyPositive `elem` options = selectBalance options bi > 0
  | COnlyNegative `elem` options = selectBalance options bi < 0
  | otherwise = True

balance queries options mbPath = (do
    coa <- getCoAItemL mbPath
    case coa of
      Leaf {..} -> byOneAccount queries options leafData
      _         -> byGroup queries options coa )
  `catchWithSrcLoc`
    (\l (e :: InvalidPath) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: NoSuchRate) -> handler l e)

byOneAccount queries options acc = do
    results <- forM queries $ \qry -> runAtomically $ getBalanceAt (qEnd qry) AvailableBalance acc
    let ends   = map qEnd   queries
    let format = case needCSV options of
                   Nothing  -> tableColumns ASCII
                   Just sep -> tableColumns (CSV sep)
    wrapIO $ putStr $ unlines $
             format [(["DATE"],    ALeft, map showMaybeDate ends),
                     (["BALANCE"], ARight, map show results)]

byGroup queries options coa = do
    let btype = if CBothBalances `elem` options
                  then BothBalances
                  else if CLedgerBalances `elem` options
                        then Only LedgerBalance
                        else Only AvailableBalance
    results <- treeBalances btype queries coa
    let filteredResults
          | (COnlyPositive `elem` options) ||
            (COnlyNegative `elem` options)  = filterLeafs (any (byBalance options)) results
          | CNoZeros `elem` options = filterLeafs (any isNotZeroBI) results
          | otherwise = results
    let results'
          | CAbsoluteValues `elem` options = mapTree (map absBI) (map absBI) filteredResults
          | otherwise = filteredResults
    let showQry = showMaybeDate . qEnd
    let format = case needCSV options of
                   Nothing  -> showTreeList ["ACCOUNT"] showI showBI options
                   Just sep -> \n qs rs -> unlines $ tableColumns (CSV sep) (treeTable showQry showBI options n qs rs)

    wrapIO $ putStr $ format (length queries) queries results'

