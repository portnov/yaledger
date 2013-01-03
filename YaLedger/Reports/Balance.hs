{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies, RecordWildCards #-}

module YaLedger.Reports.Balance
  (Balances (..)) where

import YaLedger.Reports.API

data Balances = Balances

data BOptions =
    BNoZeros
  | BHideGroups
  | BLedgerBalances
  | BBothBalances
  | BCSV (Maybe String)
  deriving (Eq)

instance ReportClass Balances where
  type Options Balances = BOptions
  type Parameters Balances = Maybe Path
  reportOptions _ = 
    [Option "z" ["no-zeros"] (NoArg BNoZeros) "Do not show accounts with zero balance",
     Option "g" ["hide-groups"] (NoArg BHideGroups) "Hide accounts groups in CSV output",
     Option "l" ["ledger"] (NoArg BLedgerBalances) "Show ledger balances instead of available balances",
     Option "b" ["both"] (NoArg BBothBalances) "Show both available and ledger balances",
     Option "C" ["csv"] (OptArg BCSV "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)"]
  defaultOptions _ = []
  reportHelp _ = "Show accounts balances. One optional parameter: account or accounts group."

  runReport _ qry opts mbPath = balance [qry] opts mbPath
  runReportL _ queries opts mbPath = balance queries opts mbPath

needCSV :: [BOptions] -> Maybe (Maybe String)
needCSV opts =
  case [s | BCSV s <- opts] of
    [] -> Nothing
    (x:_) -> Just x

showTreeList n qrys tree =
  let struct = showTreeStructure tree
      cols = [map (\l -> show (l !! i)) (allNodes tree) | i <- [0..n-1]]
  in  unlines $ tableColumns ASCII $
              (["ACCOUNT"], ALeft, struct):
              [(showI qry, ARight, col) | (col,qry) <- zip cols qrys]

treeTable b n qrys tree =
  let paths = map (intercalate "/") $ getPaths tree
      cols = [map (\l -> show (l !! i)) (getNodes tree) | i <- [0..n-1]]

      getPaths = if b then allLeafPaths else allPaths
      getNodes = if b then allLeafs else allNodes
  in  (["ACCOUNT"], ALeft, paths):
      [([showMaybeDate $ qEnd qry], ALeft, col) | (col, qry) <- zip cols qrys]
   
showI :: Query -> [String]
showI qry = [showD "now" (qEnd qry)]
  where
    showD s Nothing = s
    showD _ (Just date) = showDate date

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
    wrapIO $ putStrLn $ unlines $
             format [(["DATE"],    ALeft, map showMaybeDate ends),
                     (["BALANCE"], ARight, map show results)]

byGroup queries options coa = do
    let btype = if BBothBalances `elem` options
                  then BothBalances
                  else if BLedgerBalances `elem` options
                        then Only LedgerBalance
                        else Only AvailableBalance
    results <- treeBalances btype queries coa
    let results' = if BNoZeros `elem` options
                     then filterLeafs (any isNotZeroBI) results
                     else results
    let hideGroups = BHideGroups `elem` options
    let format = case needCSV options of
                   Nothing  -> showTreeList
                   Just sep -> \n qs rs -> unlines $ tableColumns (CSV sep) (treeTable hideGroups n qs rs)

    wrapIO $ putStrLn $ format (length queries) queries results'

