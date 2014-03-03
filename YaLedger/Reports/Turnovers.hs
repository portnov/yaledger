{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, RecordWildCards, TypeFamilies #-}

module YaLedger.Reports.Turnovers where

import YaLedger.Reports.API

data Turnovers = Turnovers

data TOptions =
    TNoZeros
  | TLedgerBalances
  | TBothBalances
  | TShowTotals
  | TShowSaldo
  | Common CommonFlags
  deriving (Eq)

instance ReportClass Turnovers where
  type Options Turnovers = TOptions
  type Parameters Turnovers = Maybe Path
  defaultOptions Turnovers = []
  reportHelp _ = "Show debit/credit, and, optionally, total turnovers for each account."
  reportOptions _ =
    [Option "l" ["ledger"] (NoArg TLedgerBalances) "Show ledger balances instead of available balances",
     Option "b" ["both"] (NoArg TBothBalances) "Show both available and ledger balances",
     Option "z" ["no-zeros"] (NoArg TNoZeros) "Do not show accounts with zero balance",
     Option "t" ["show-totals"] (NoArg TShowTotals) "Show total turnovers",
     Option "s" ["show-saldo"] (NoArg TShowSaldo) "Show saldo (credit - debit)",
     Option "R" ["rategroup"] (ReqArg (Common . CRateGroup) "GROUP") "Use specified exchange rates group for account groups balances calculation, instead of default",
     Option "C" ["csv"] (OptArg (Common . CCSV) "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)",
     Option "H" ["html"] (NoArg (Common CHTML)) "Output data in HTML format"]

  initReport _ options _ = setOutputFormat (commonFlags options)

  runReportL _ queries opts mbPath =
    turnoversL queries opts mbPath
      `catchWithSrcLoc`
        (\l (e :: InvalidPath) -> handler l e)
      `catchWithSrcLoc`
        (\l (e :: NoSuchRate) -> handler l e)

  runReport _ qry opts mbPath =
    turnoversL [qry] opts mbPath
      `catchWithSrcLoc`
        (\l (e :: InvalidPath) -> handler l e)
      `catchWithSrcLoc`
        (\l (e :: NoSuchRate) -> handler l e)

data TRecord v = TRecord {
  trCredit :: v,
  trDebit  :: v,
  trIncSaldo :: BalanceInfo v,
  trOutSaldo :: BalanceInfo v,
  trTotals   :: Maybe v,
  trSaldo    :: Maybe v }
  deriving (Eq, Show)

commonFlags :: [TOptions] -> [CommonFlags]
commonFlags opts = [flag | Common flag <- opts]

sumTurnovers mbDate rgroup calcTotals calcSaldo ag list = do
  let c = agCurrency ag
  list' <- forM list $ \tr -> do
                cr' :# _  <- convert mbDate rgroup c (trCredit tr)
                dt' :# _  <- convert mbDate rgroup c (trDebit tr)
                inc' <- convertBalanceInfo mbDate rgroup c (trIncSaldo tr)
                out' <- convertBalanceInfo mbDate rgroup c (trOutSaldo tr)
                t'  <- case trTotals tr of
                         Nothing -> return Nothing
                         Just t -> do
                                   x :# _ <- convert mbDate rgroup c t
                                   return (Just x)
                s <- case trSaldo tr of
                       Nothing -> return Nothing
                       Just s -> do
                                 x :# _ <- convert mbDate rgroup c s
                                 return (Just x)
                return $ TRecord {
                           trCredit = cr',
                           trDebit  = dt',
                           trIncSaldo = inc',
                           trOutSaldo = out',
                           trTotals = t',
                           trSaldo  = s }
  let credits = sum $ map trCredit list'
      debits  = sum $ map trDebit  list'
      inc     = sumBalanceInfo c $ map trIncSaldo list'
      out     = sumBalanceInfo c $ map trOutSaldo list'
      totals  = if calcTotals
                  then Just $ (sum $ map (fromJust . trTotals) list') :# c
                  else Nothing
      saldo   = if calcSaldo
                  then Just $ (sum $ map (fromJust . trSaldo) list') :# c
                  else Nothing
  return $ TRecord {
             trCredit = credits :# c,
             trDebit  = debits  :# c,
             trIncSaldo = inc,
             trOutSaldo = out,
             trTotals = totals,
             trSaldo  = saldo }

allTurnovers bqry calcTotals calcSaldo qry account = do
  cr :# c <- creditTurnovers qry account
  dt :# _ <- debitTurnovers  qry account
  opts <- gets lsConfig
  incomingSaldo <- case qStart qry of
                     Nothing    -> return $ BalanceInfo (Just 0) (Just 0)
                     Just start -> runAtomically $ getBalanceInfoAt (Just start) bqry account
  outgoingSaldo <- runAtomically $ getBalanceInfoAt (qEnd qry) bqry account
  return $ TRecord {
             trCredit = cr :# c,
             trDebit  = dt :# c,
             trIncSaldo = balanceInfoSetCurrency incomingSaldo c,
             trOutSaldo = balanceInfoSetCurrency outgoingSaldo c,
             trTotals = if calcTotals
                          then Just $ (cr + dt) :# c
                          else Nothing ,
             trSaldo  = if calcSaldo
                          then Just $ if isAssets opts (getAttrs account)
                                        then (dt - cr) :# c
                                        else (cr - dt) :# c
                          else Nothing }

noZeroTurns tr =
  case trTotals tr of
    Nothing -> isNotZero (trCredit tr) || isNotZero (trDebit tr)
    Just t  -> isNotZero t

turnoversL :: (Throws InvalidPath l,
               Throws NoSuchRate l,
               Throws InternalError l)
           => [Query] -> [TOptions] -> Maybe Path -> Ledger l ()
turnoversL queries options mbPath = do
    coa <- getCoAItemL mbPath
    case coa of
      Leaf {..} -> byOneAccount queries options leafData
      _ -> forM_ queries $ \qry -> do
                outputText $ showInterval qry
                turnovers qry options coa

byOneAccount queries options account = do
  let calcTotals = TShowTotals `elem` options
      calcSaldo  = TShowSaldo  `elem` options
      bqry = if TBothBalances `elem` options
                then BothBalances
                else if TLedgerBalances `elem` options
                      then Only LedgerBalance
                      else Only AvailableBalance
  turns <- forM queries $ \qry ->
               allTurnovers bqry calcTotals calcSaldo qry account
  let check
        | TNoZeros `elem` options = noZeroTurns
        | otherwise               = const True
      xs = [(tr,qry) | (tr,qry) <- zip turns queries, check tr]
      turns'   = map fst xs
      queries' = map snd xs 
  let credits = map trCredit turns'
      debits  = map trDebit  turns'
      inc     = map trIncSaldo turns'
      out     = map trOutSaldo turns'
      totals  = map trTotals   turns'
      saldo   = map trSaldo    turns'
      starts  = map qStart queries'
      ends    = map qEnd   queries'
  oformat <- getOutputFormat
  let format = case oformat of
                 OASCII ASCII -> tableColumns ASCII
                 OCSV csv   -> tableColumns csv
                 OHTML html -> tableColumns html
  outputText $ unlinesText $ format $
      [([output "FROM"], ALeft, map showMaybeDate starts),
       ([output "TO"],   ALeft, map showMaybeDate ends),
       ([output "BALANCE C/F"], ARight, map prettyPrint inc),
       ([output "DEBIT"],       ARight, map prettyPrint debits),
       ([output "CREDIT"],      ARight, map prettyPrint credits),
       ([output "BALANCE B/D"], ARight, map prettyPrint out)] ++
      (if calcTotals
         then [([output "TOTALS"], ARight, map (prettyPrint . fromJust) totals)]
         else []) ++
      (if calcSaldo
         then [([output "SALDO"],  ARight, map (prettyPrint . fromJust) saldo)]
         else [])

turnovers qry options coa = do
  let calcTotals = TShowTotals `elem` options
      calcSaldo  = TShowSaldo  `elem` options
      bqry = if TBothBalances `elem` options
                then BothBalances
                else if TLedgerBalances `elem` options
                      then Only LedgerBalance
                      else Only AvailableBalance
      rgroup = selectRateGroup (commonFlags options)
  tree <- mapTreeM (sumTurnovers (qEnd qry) rgroup calcTotals calcSaldo) (allTurnovers bqry calcTotals calcSaldo qry) coa
  let tree' = if TNoZeros `elem` options
                then filterLeafs noZeroTurns tree
                else tree
  oformat <- getOutputFormat
  let struct = case oformat of
                 OASCII _ -> showTreeStructure tree'
                 OCSV _  -> map (output . intercalate "/") (allPaths tree')
                 OHTML _ -> showTreeStructure tree'
      nodes = allNodes tree'
      credits = map trCredit nodes
      debits  = map trDebit  nodes
      inc     = map trIncSaldo nodes
      out     = map trOutSaldo nodes
      totals  = map trTotals nodes
      saldo   = map trSaldo  nodes
  oformat <- getOutputFormat
  let format = case oformat of
                 OASCII ASCII -> tableColumns ASCII
                 OCSV csv   -> tableColumns csv
                 OHTML html -> tableColumns html
  outputText $ unlinesText $ format $
      [([output "ACCOUNT"],     ALeft, struct),
       ([output "BALANCE C/F"], ARight, map prettyPrint inc),
       ([output "DEBIT"],       ARight, map prettyPrint debits),
       ([output "CREDIT"],      ARight, map prettyPrint credits),
       ([output "BALANCE B/D"], ARight,  map prettyPrint out)] ++
      (if calcTotals
         then [([output "TOTALS"], ARight, map (prettyPrint . fromJust) totals)]
         else []) ++
      (if calcSaldo
         then [([output "SALDO"],  ARight, map (prettyPrint . fromJust) saldo)]
         else [])
                  
