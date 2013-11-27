{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies, RecordWildCards, TemplateHaskell #-}

module YaLedger.Reports.Balance
  (Balances (..)) where

import YaLedger.Reports.API

data Balances = Balances

data BOptions =
        Twoside
      | UseCurrency String
      | SecondCurrency String
      | Common CommonFlags
  deriving (Eq, Show)

instance ReportClass Balances where
  type Options Balances = BOptions
  type Parameters Balances = Maybe Path
  reportOptions _ = 
    [Option "z" ["no-zeros"] (NoArg $ Common CNoZeros) "Do not show accounts with zero balance",
     Option "p" ["positive"] (NoArg $ Common COnlyPositive) "Show only accounts with positive balance",
     Option "n" ["negative"] (NoArg $ Common COnlyNegative) "Show only accounts with negative balance",
     Option "a" ["absolute"] (NoArg $ Common CAbsoluteValues) "Show absolute values of all balances",
     Option "g" ["hide-groups"] (NoArg $ Common CHideGroups) "Hide accounts groups in CSV output",
     Option "c" ["currency"] (ReqArg UseCurrency "CURRENCY") "Show all amounts in given CURRENCY",
     Option "P" ["second-currency"] (ReqArg SecondCurrency "CURRENCY") "Show all amounts also in given CURRENCY",
     Option ""  ["no-currencies"] (NoArg $ Common CNoCurrencies) "Do not show currencies in amounts",
     Option "l" ["ledger"] (NoArg $ Common CLedgerBalances) "Show ledger balances instead of available balances",
     Option "b" ["both"] (NoArg $ Common CBothBalances) "Show both available and ledger balances",
     Option "t" ["twoside"] (NoArg Twoside) "Show twoside (assets/liablities) report",
     Option "C" ["csv"] (OptArg (Common . CCSV) "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)",
     Option "H" ["html"] (NoArg (Common CHTML)) "Output data in HTML format"]
  defaultOptions _ = []
  reportHelp _ = "Show accounts balances. One optional parameter: account or accounts group."

  initReport _ options _ = setOutputFormat (commonFlags options)

  runReport _ qry opts mbPath = balance [qry] opts mbPath
  runReportL _ queries opts mbPath = balance queries opts mbPath

commonFlags :: [BOptions] -> [CommonFlags]
commonFlags opts = [flag | Common flag <- opts]

showI :: Query -> [FormattedText]
showI qry = [showD "now" (qEnd qry)]
  where
    showD s Nothing = output s
    showD _ (Just date) = output $ showDate date

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
      _         -> if Twoside `elem` options
                     then forM_ queries $ \qry -> do
                              outputText $ showInterval qry
                              twosideReport qry options coa
                     else byGroup queries (commonFlags options) coa )
  `catchWithSrcLoc`
    (\l (e :: InvalidPath) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: NoSuchRate) -> handler l e)

byOneAccount queries options acc = do
    currency <- case [str | UseCurrency str <- options] of
                  [] -> return $ getCurrency acc
                  (c:_) -> currencyByName c
    balancesSrc <- forM queries $ \qry -> runAtomically $ getBalanceAt (qEnd qry) AvailableBalance acc
    let balancesAmts = map (:# getCurrency acc) balancesSrc
    balances <- if currency == getCurrency acc
                  then return balancesAmts
                  else zipWithM (\qry amt -> convert (qEnd qry) currency amt) queries balancesAmts
    currency2 <- case [str | SecondCurrency str <- options] of
                   [] -> return Nothing
                   (c:_) -> Just <$> currencyByName c
    balances2 <- case currency2 of
                   Nothing -> return balancesAmts
                   Just ccy2 -> zipWithM (\qry amt -> convert (qEnd qry) ccy2 amt) queries balancesAmts
    let ends = map qEnd queries
    let hideCcys = CNoCurrencies `elem` commonFlags options
    let format = case selectOutputFormat (commonFlags options) of
                   OASCII _ -> tableColumns ASCII
                   OCSV csv -> tableColumns csv
                   OHTML html -> tableColumns html
    let results = if CNoZeros `elem` commonFlags options
                    then [(end, bal, bal2) | (end, bal@(x :#_), bal2) <- zip3 ends balances balances2, x /= 0]
                    else zip3 ends balances balances2
        ends'      = [ end | (end, _, _) <- results ]
        balances'  = [ bal | (_, bal, _) <- results ]
        balances2' = [ bal2 | (_, _, bal2) <- results ]
    outputText $ unlinesText $
             format $ [([output "DATE"],    ALeft, map showMaybeDate ends'),
                       ([output "BALANCE"], ARight, map (printAmt hideCcys) balances' )] ++
                      case currency2 of
                        Nothing -> []
                        Just c -> [([output $ "IN " ++ show c], ARight, map (printAmt hideCcys) balances2')]

printAmt False amt = prettyPrint amt
printAmt True (x :# _) = prettyPrint x

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
    let showQry q = [showMaybeDate $ qEnd q]
    let format = case selectOutputFormat options of
                   OASCII _ -> \n qs rs -> unlinesText $ showTreeList [output "ACCOUNT"] showI showBI options n qs rs
                   OCSV csv -> \n qs rs -> unlinesText $ tableColumns csv (treeTable showQry showBI options n qs rs)
                   OHTML html -> \n qs rs -> unlinesText $ showTreeList [output "ACCOUNT"] showI showBI options n qs rs

    outputText $ format (length queries) queries results'

twosideReport qry options coa = do
    opts <- gets lsConfig
    let flags = commonFlags options
    let btype = if CBothBalances `elem` flags
                  then BothBalances
                  else if CLedgerBalances `elem` flags
                        then Only LedgerBalance
                        else Only AvailableBalance
    let filtered rs
          | (COnlyPositive `elem` flags) ||
            (COnlyNegative `elem` flags)  = filterLeafs (any (byBalance flags)) rs
          | CNoZeros `elem` flags = filterLeafs (any isNotZeroBI) rs
          | otherwise = rs
    let prepare rs
          | CAbsoluteValues `elem` flags = mapTree (map absBI) (map absBI) rs
          | otherwise = rs
    let assets      = filterLeafs (isAssets opts      . accountAttributes) coa
        liabilities = filterLeafs (not . isAssets opts . accountAttributes) coa
    if isEmptyTree assets || isEmptyTree liabilities
      then byGroup [qry] flags coa
      else do
            assetsResults      <- treeBalances btype [qry] assets
            liabilitiesResults <- treeBalances btype [qry] liabilities
            let balColumn rs = map (\l -> showBI flags (head l)) (allNodes rs)
            let format as ls =
                  let structAs = showTreeStructure as
                      structLs = showTreeStructure ls
                      deltaLen = length structAs - length structLs
                      empties = replicate (abs deltaLen) emptyText
                      emptyAs = if deltaLen > 0 then [] else empties
                      emptyLs = if deltaLen < 0 then [] else empties
                  in case selectOutputFormat flags of
                       OASCII _  ->  tableColumns ASCII $
                                                   [([output "ACCOUNT"], ALeft,  structAs ++ emptyAs),
                                                    ([output "ASSETS"],  ARight, balColumn as ++ emptyAs),
                                                    ([output "ACCOUNT"], ALeft, structLs ++ emptyLs),
                                                    ([output "LIABILITIES"], ARight, balColumn ls ++ emptyLs)]
                       OCSV csv -> tableColumns csv $
                                                   [([output "ACCOUNT"], ALeft,  structAs ++ emptyAs),
                                                    ([output "ASSETS"],  ARight, balColumn as ++ emptyAs),
                                                    ([output "ACCOUNT"], ALeft, structLs ++ emptyLs),
                                                    ([output "LIABILITIES"], ARight, balColumn ls ++ emptyLs)]
                       OHTML html -> tableColumns html $
                                                   [([output "ACCOUNT"], ALeft,  structAs ++ emptyAs),
                                                    ([output "ASSETS"],  ARight, balColumn as ++ emptyAs),
                                                    ([output "ACCOUNT"], ALeft, structLs ++ emptyLs),
                                                    ([output "LIABILITIES"], ARight, balColumn ls ++ emptyLs)]

            outputText $ unlinesText $ format
                                          (prepare $ filtered assetsResults)
                                          (prepare $ filtered liabilitiesResults)

