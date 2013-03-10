{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies, RecordWildCards #-}

module YaLedger.Reports.Saldo
  (Saldo (..)) where

import YaLedger.Reports.API

data Saldo = Saldo

type SOptions = CommonFlags

instance ReportClass Saldo where
  type Options Saldo = SOptions
  type Parameters Saldo = Maybe Path
  reportOptions _ = 
    [Option "z" ["no-zeros"] (NoArg CNoZeros) "Do not show accounts with zero balance",
     Option "p" ["positive"] (NoArg COnlyPositive) "Show only accounts with positive balance",
     Option "n" ["negative"] (NoArg COnlyNegative) "Show only accounts with negative balance",
     Option "a" ["absolute"] (NoArg CAbsoluteValues) "Show absolute values of all balances",
     Option "g" ["hide-groups"] (NoArg CHideGroups) "Hide accounts groups in CSV output",
     Option ""  ["no-currencies"] (NoArg CNoCurrencies) "Do not show currencies in amounts",
     Option "C" ["csv"] (OptArg CCSV "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)"]
  defaultOptions _ = []
  reportHelp _ = "Show accounts balances. One optional parameter: account or accounts group."

  runReport _ qry opts mbPath = getSaldo [qry] opts mbPath
  runReportL _ queries opts mbPath = getSaldo queries opts mbPath

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

onlyPositive :: [Amount] -> [Amount]
onlyPositive = map go
  where go a@(x :# c)
          | x > 0 = a
          | otherwise = 0 :# c

onlyNegative :: [Amount] -> [Amount]
onlyNegative = map go
  where go a@(x :# c)
          | x < 0 = a
          | otherwise = 0 :# c

mbAbs options = if CAbsoluteValues `elem` options
            then map absAmount
            else id

byOneAccount queries options acc = do
    results <- forM queries $ \qry -> saldo qry acc
    let starts = map qStart queries
        ends   = map qEnd   queries
    let prepare
          | COnlyPositive `elem` options = onlyPositive
          | COnlyNegative `elem` options = mbAbs options . onlyNegative
          | CAbsoluteValues `elem` options = map absAmount
          | otherwise = id
    totals <- runAtomically $ getCurrentBalance AvailableBalance acc
    let format = case needCSV options of
                   Nothing  -> tableColumns ASCII
                   Just sep -> tableColumns (CSV sep)
    let footer = case needCSV options of
                   Nothing -> ["    TOTALS: " ++ show totals ++ show (getCurrency acc)]
                   _ -> []
    wrapIO $ putStr $ unlines $
             format [(["FROM"],    ALeft, map showMaybeDate starts),
                     (["TO"],      ALeft, map showMaybeDate ends),
                     (["BALANCE"], ARight, map (showAmt options) $ prepare results)] ++ footer

byGroup queries options coa = do
    results <- treeSaldos queries coa
    let prepare
          | COnlyPositive `elem` options = mapTree onlyPositive onlyPositive
          | COnlyNegative `elem` options = mapTree (mbAbs options . onlyNegative) (mbAbs options . onlyNegative)
          | CAbsoluteValues `elem` options = mapTree (map absAmount) (map absAmount)
          | otherwise = id
    let results' = if CNoZeros `elem` options
                     then filterLeafs (any isNotZero) results
                     else results
    let hideGroups = CHideGroups `elem` options
    let format = case needCSV options of
                   Nothing  -> showTreeList ["", "ACCOUNT", ""] showI (const show) options
                   Just sep -> \n qs rs -> unlines $ tableColumns (CSV sep) (treeTable showInterval showAmt options n qs rs)

    wrapIO $ putStr $ format (length queries) queries (prepare results')

