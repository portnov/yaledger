{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies, RecordWildCards #-}

module YaLedger.Reports.Saldo
  (Saldo (..)) where

import YaLedger.Reports.API

data Saldo = Saldo

data SOptions =
    SNoZeros
  | SCSV (Maybe String)
  deriving (Eq)

instance ReportClass Saldo where
  type Options Saldo = SOptions
  type Parameters Saldo = Maybe Path
  reportOptions _ = 
    [Option "z" ["no-zeros"] (NoArg SNoZeros) "Do not show accounts with zero balance",
     Option "C" ["csv"] (OptArg SCSV "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)"]
  defaultOptions _ = []
  reportHelp _ = "Show accounts balances. One optional parameter: account or accounts group."

  runReport _ qry opts mbPath = getSaldo [qry] opts mbPath
  runReportL _ queries opts mbPath = getSaldo queries opts mbPath

needCSV :: [SOptions] -> Maybe (Maybe String)
needCSV opts =
  case [s | SCSV s <- opts] of
    [] -> Nothing
    (x:_) -> Just x

showTreeList n qrys tree =
  let struct = showTreeStructure tree
      cols = [map (\l -> show (l !! i)) (allNodes tree) | i <- [0..n-1]]
  in  unlines $ tableColumns ASCII $
              (["","ACCOUNT",""], ALeft, struct):
              [(showI qry, ARight, col) | (col,qry) <- zip cols qrys]

treeTable n qrys tree =
  let paths = map (intercalate "/") $ allPaths tree
      cols = [map (\l -> show (l !! i)) (allNodes tree) | i <- [0..n-1]]
  in  (["ACCOUNT"], ALeft, paths):
      [([showInterval qry], ALeft, col) | (col, qry) <- zip cols qrys]
   

showI :: Query -> [String]
showI qry = [showD "beginning" (qStart qry), "...", showD "now" (qEnd qry)]
  where
    showD s Nothing = s
    showD _ (Just date) = showDate date

getSaldo queries options mbPath = (do
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
    results <- forM queries $ \qry -> saldo qry acc
    let starts = map qStart queries
        ends   = map qEnd   queries
    totals <- getCurrentBalance AvailableBalance acc
    let format = case needCSV options of
                   Nothing  -> tableColumns ASCII
                   Just sep -> tableColumns (CSV sep)
    wrapIO $ putStrLn $ unlines $
             format [(["FROM"],    ALeft, map showMaybeDate starts),
                     (["TO"],      ALeft, map showMaybeDate ends),
                     (["BALANCE"], ARight, map show results)] ++ 
             ["    TOTALS: " ++ show totals ++ show (getCurrency acc)]

byGroup queries options coa = do
    results <- treeSaldos queries coa
    let results' = if SNoZeros `elem` options
                     then filterLeafs (any isNotZero) results
                     else results
    let format = case needCSV options of
                   Nothing  -> showTreeList
                   Just sep -> \n qs rs -> unlines $ tableColumns (CSV sep) (treeTable n qs rs)

    wrapIO $ putStrLn $ format (length queries) queries results'

