{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies, RecordWildCards #-}

module YaLedger.Reports.Balance where

import YaLedger.Reports.API

data Balances = Balances

data BOptions =
    BNoZeros
  | BCSV (Maybe String)
  deriving (Eq)

instance ReportClass Balances where
  type Options Balances = BOptions
  type Parameters Balances = Maybe Path
  reportOptions _ = 
    [Option "z" ["no-zeros"] (NoArg BNoZeros) "Do not show accounts with zero balance",
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
  in  unlines $ columns $ (["","ACCOUNT",""], ALeft, struct):
                          [(showI qry, ARight, col) | (col,qry) <- zip cols qrys]

treeTable n qrys tree =
  let paths = map (intercalate "/") $ allPaths tree
      cols = [map (\l -> show (l !! i)) (allNodes tree) | i <- [0..n-1]]
  in  ("ACCOUNT", paths):
      [(showInterval qry, col) | (col, qry) <- zip cols qrys]
   

showI :: Query -> [String]
showI qry = [showD "beginning" (qStart qry), "...", showD "now" (qEnd qry)]
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
    results <- forM queries $ \qry -> saldo qry acc
    let starts = map qStart queries
        ends   = map qEnd   queries
    totals <- getCurrentBalance acc
    case needCSV options of
      Nothing ->
        wrapIO $ putStrLn $ unlines $
                 columns [(["FROM"],    ALeft, map showMaybeDate starts),
                          (["TO"],      ALeft, map showMaybeDate ends),
                          (["BALANCE"], ARight, map show results)] ++ 
                 ["    TOTALS: " ++ show totals ++ show (getCurrency acc)]
      Just mbSep -> do
        let sep = fromMaybe ";" mbSep
        wrapIO $ putStrLn $ csvColumns sep $ [
              ("FROM", map showMaybeDate starts),
              ("TO",   map showMaybeDate ends),
              ("BALANCE", map show results) ]

byGroup queries options coa = do
    results <- treeSaldos queries coa
    let results' = if BNoZeros `elem` options
                     then filterLeafs (any isNotZero) results
                     else results
    case needCSV options of
      Nothing ->
        wrapIO $ putStrLn $ showTreeList (length queries) queries results'
      Just mbSep -> do
        let sep = fromMaybe ";" mbSep
        wrapIO $ putStrLn $ csvColumns sep $ treeTable (length queries) queries results'
               

