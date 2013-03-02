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

needCSV :: [BOptions] -> Maybe (Maybe String)
needCSV opts =
  case [s | CCSV s <- opts] of
    [] -> Nothing
    (x:_) -> Just x

showTreeList :: [CommonFlags] -> Int -> [Query] -> Tree [BalanceInfo Amount] [BalanceInfo Amount] -> String
showTreeList options n qrys tree =
  let struct = showTreeStructure tree
      cols = [map (\l -> showBI options (l !! i)) (allNodes tree) | i <- [0..n-1]]
  in  unlines $ tableColumns ASCII $
              (["ACCOUNT"], ALeft, struct):
              [(showI qry, ARight, col) | (col,qry) <- zip cols qrys]

treeTable :: [CommonFlags] -> Int -> [Query] -> Tree [BalanceInfo Amount] [BalanceInfo Amount] -> [(Column, Align, Column)]
treeTable options n qrys tree =
  let paths = map (intercalate "/") $ getPaths tree
      hideGroups = CHideGroups `elem` options
      cols = [map (\l -> showBI options (l !! i)) (getNodes tree) | i <- [0..n-1]]
      getPaths = if hideGroups then allLeafPaths else allPaths
      getNodes = if hideGroups then allLeafs else allNodes
  in  (["ACCOUNT"], ALeft, paths):
      [([showMaybeDate $ qEnd qry], ALeft, col) | (col, qry) <- zip cols qrys]
   
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
    coa <- case mbPath of
              Nothing   -> gets lsCoA
              Just path -> getCoAItem (gets lsPosition) (gets lsCoA) path
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
    let format = case needCSV options of
                   Nothing  -> showTreeList options
                   Just sep -> \n qs rs -> unlines $ tableColumns (CSV sep) (treeTable options n qs rs)

    wrapIO $ putStr $ format (length queries) queries results'

