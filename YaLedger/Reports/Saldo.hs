{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies, RecordWildCards #-}

module YaLedger.Reports.Saldo
  (Saldo (..)) where

import YaLedger.Reports.API

data Saldo = Saldo

data SOptions =
       Twoside
     | Common CommonFlags
  deriving (Eq, Show)

instance ReportClass Saldo where
  type Options Saldo = SOptions
  type Parameters Saldo = Maybe Path
  reportOptions _ = 
    [Option "z" ["no-zeros"] (NoArg $ Common CNoZeros) "Do not show accounts with zero balance",
     Option "p" ["positive"] (NoArg $ Common COnlyPositive) "Show only accounts with positive balance",
     Option "n" ["negative"] (NoArg $ Common COnlyNegative) "Show only accounts with negative balance",
     Option "a" ["absolute"] (NoArg $ Common CAbsoluteValues) "Show absolute values of all balances",
     Option "g" ["hide-groups"] (NoArg $ Common CHideGroups) "Hide accounts groups in CSV output",
     Option ""  ["no-currencies"] (NoArg $ Common CNoCurrencies) "Do not show currencies in amounts",
     Option "t" ["twoside"] (NoArg Twoside) "Show twoside (assets/liablities) report",
     Option "C" ["csv"] (OptArg (Common . CCSV) "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)"]
  defaultOptions _ = []
  reportHelp _ = "Show accounts balances. One optional parameter: account or accounts group."

  runReport _ qry opts mbPath = getSaldo [qry] opts mbPath
  runReportL _ queries opts mbPath = getSaldo queries opts mbPath

commonFlags :: [SOptions] -> [CommonFlags]
commonFlags opts = [flag | Common flag <- opts]

showI :: Query -> [FormattedText]
showI qry = [showD "beginning" (qStart qry), output "...", showD "now" (qEnd qry)]
  where
    showD s Nothing = output s
    showD _ (Just date) = output $ showDate date

getSaldo queries options mbPath = (do
    let flags = commonFlags options
    colorize <- gets (colorizeOutput . lsConfig)
    coa <- getCoAItemL mbPath
    case coa of
      Leaf {..} -> byOneAccount queries flags leafData
      _         -> if Twoside `elem` options
                     then forM_ queries $ \qry -> do
                              wrapIO $ putTextLn colorize $ showInterval qry
                              twosideReport qry flags coa
                     else byGroup queries flags coa )
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
    colorize <- gets (colorizeOutput . lsConfig)
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
                   Nothing -> [output "    TOTALS: " <> prettyPrint totals <> show (getCurrency acc)]
                   _ -> []
    wrapIO $ putTextLn colorize $ unlinesText $
             format [([output "FROM"],    ALeft, map showMaybeDate starts),
                     ([output "TO"],      ALeft, map showMaybeDate ends),
                     ([output "BALANCE"], ARight, map (showAmt options) $ prepare results)] ++ footer

byGroup queries options coa = do
    colorize <- gets (colorizeOutput . lsConfig)
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
                   Nothing  -> \n qs rs -> unlinesText $ showTreeList [emptyText, output "ACCOUNT", emptyText] showI (const prettyPrint) options n qs rs
                   Just sep -> \n qs rs -> unlinesText $ tableColumns (CSV sep) (treeTable showInterval showAmt options n qs rs)

    wrapIO $ putTextLn colorize $ format (length queries) queries (prepare results')

twosideReport qry options coa = do
    opts <- gets lsConfig
    let prepare
          | COnlyPositive `elem` options = mapTree onlyPositive onlyPositive
          | COnlyNegative `elem` options = mapTree (mbAbs options . onlyNegative) (mbAbs options . onlyNegative)
          | CAbsoluteValues `elem` options = mapTree (map absAmount) (map absAmount)
          | otherwise = id
    let filtered rs
          | (COnlyPositive `elem` options) ||
            (COnlyNegative `elem` options)  = filterLeafs (any isNotZero) rs
          | CNoZeros `elem` options = filterLeafs (any isNotZero) rs
          | otherwise = rs
    let assets      = filterLeafs (isAssets opts      . accountAttributes) coa
        liabilities = filterLeafs (not . isAssets opts . accountAttributes) coa
    if isEmptyTree assets || isEmptyTree liabilities
      then byGroup [qry] options coa
      else do
            assetsResults      <- treeSaldos [qry] assets
            liabilitiesResults <- treeSaldos [qry] liabilities
            let balColumn rs = map (\l -> prettyPrint (head l)) (allNodes rs)
            let format as ls =
                  let structAs = showTreeStructure as
                      structLs = showTreeStructure ls
                      deltaLen = length structAs - length structLs
                      empties = replicate (abs deltaLen) emptyText
                      emptyAs = if deltaLen > 0 then [] else empties
                      emptyLs = if deltaLen < 0 then [] else empties
                  in case needCSV options of
                       Nothing  ->  tableColumns ASCII $
                                                   [([output "ACCOUNT"], ALeft,  structAs ++ emptyAs),
                                                    ([output "ASSETS"],  ARight, balColumn as ++ emptyAs),
                                                    ([output "ACCOUNT"], ALeft, structLs ++ emptyLs),
                                                    ([output "LIABILITIES"], ARight, balColumn ls ++ emptyLs)]
                       Just sep -> tableColumns (CSV sep) $
                                                   [([output "ACCOUNT"], ALeft,  structAs ++ emptyAs),
                                                    ([output "ASSETS"],  ARight, balColumn as ++ emptyAs),
                                                    ([output "ACCOUNT"], ALeft, structLs ++ emptyLs),
                                                    ([output "LIABILITIES"], ARight, balColumn ls ++ emptyLs)]

            wrapIO $ putTextLn (colorizeOutput opts) $ unlinesText $ format
                                          (filtered $ prepare assetsResults)
                                          (filtered $ prepare liabilitiesResults)

