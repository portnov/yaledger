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
  | TCSV (Maybe String)
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
     Option "C" ["csv"] (OptArg TCSV "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)" ]

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

sumTurnovers mbDate calcTotals calcSaldo ag list = do
  let c = agCurrency ag
  list' <- forM list $ \tr -> do
                cr' :# _  <- convert mbDate c (trCredit tr)
                dt' :# _  <- convert mbDate c (trDebit tr)
                inc' <- convertBalanceInfo mbDate c (trIncSaldo tr)
                out' <- convertBalanceInfo mbDate c (trOutSaldo tr)
                t'  <- case trTotals tr of
                         Nothing -> return Nothing
                         Just t -> do
                                   x :# _ <- convert mbDate c t
                                   return (Just x)
                s <- case trSaldo tr of
                       Nothing -> return Nothing
                       Just s -> do
                                 x :# _ <- convert mbDate c s
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
                wrapIO $ putStrLn $ showInterval qry
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
  let format = case [s | TCSV s <- options] of
                 []    -> tableColumns ASCII
                 (x:_) -> tableColumns (CSV x)
  wrapIO $ putStrLn $ unlines $ format $
      [(["FROM"], ALeft, map showMaybeDate starts),
       (["TO"],   ALeft, map showMaybeDate ends),
       (["BALANCE C/F"], ARight, map show inc),
       (["DEBIT"],       ARight, map show debits),
       (["CREDIT"],      ARight, map show credits),
       (["BALANCE B/D"], ARight, map show out)] ++
      (if calcTotals
         then [(["TOTALS"], ARight, map (show . fromJust) totals)]
         else []) ++
      (if calcSaldo
         then [(["SALDO"],  ARight, map (show . fromJust) saldo)]
         else [])

turnovers qry options coa = do
  let calcTotals = TShowTotals `elem` options
      calcSaldo  = TShowSaldo  `elem` options
      bqry = if TBothBalances `elem` options
                then BothBalances
                else if TLedgerBalances `elem` options
                      then Only LedgerBalance
                      else Only AvailableBalance
  tree <- mapTreeM (sumTurnovers (qEnd qry) calcTotals calcSaldo) (allTurnovers bqry calcTotals calcSaldo qry) coa
  let tree' = if TNoZeros `elem` options
                then filterLeafs noZeroTurns tree
                else tree
  let struct = case [s | TCSV s <- options] of
                 [] -> showTreeStructure tree'
                 _  -> map (intercalate "/") (allPaths tree')
      nodes = allNodes tree'
      credits = map trCredit nodes
      debits  = map trDebit  nodes
      inc     = map trIncSaldo nodes
      out     = map trOutSaldo nodes
      totals  = map trTotals nodes
      saldo   = map trSaldo  nodes
  let format = case [s | TCSV s <- options] of
                 []    -> tableColumns ASCII
                 (x:_) -> tableColumns (CSV x)
  wrapIO $ putStrLn $ unlines $ format $
      [(["ACCOUNT"],     ALeft, struct),
       (["BALANCE C/F"], ARight, map show inc),
       (["DEBIT"],       ARight, map show debits),
       (["CREDIT"],      ARight, map show credits),
       (["BALANCE B/D"], ARight,  map show out)] ++
      (if calcTotals
         then [(["TOTALS"], ARight, map (show . fromJust) totals)]
         else []) ++
      (if calcSaldo
         then [(["SALDO"],  ARight, map (show . fromJust) saldo)]
         else [])
                  
