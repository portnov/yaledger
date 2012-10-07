{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, RecordWildCards, TypeFamilies #-}
{- # OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.Turnovers where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Data.Maybe

import YaLedger.Types
import YaLedger.Types.Reports
import YaLedger.Strings
import YaLedger.Kernel
import YaLedger.Monad
import YaLedger.Exceptions
import YaLedger.Logger
import YaLedger.Reports.Common

data Turnovers = Turnovers

data TOptions = TNoZeros | TShowTotals
  deriving (Eq)

instance ReportClass Turnovers where
  type Options Turnovers = TOptions
  type Parameters Turnovers = Maybe Path
  defaultOptions Turnovers = []
  reportHelp _ = "Show debit/credit, and, optionally, total turnovers for each account."
  reportOptions _ =
    [Option "z" ["no-zeros"] (NoArg TNoZeros) "Do not show accounts with zero balance",
     Option "t" ["show-totals"] (NoArg TShowTotals) "Show total turnovers" ]
  runReport _ qry opts mbPath =
    turnovers' qry opts mbPath
      `catchWithSrcLoc`
        (\l (e :: InvalidPath) -> handler l e)
      `catchWithSrcLoc`
        (\l (e :: NoSuchRate) -> handler l e)

data TRecord v = TRecord {
  trCredit :: v,
  trDebit  :: v,
  trIncSaldo :: v,
  trOutSaldo :: v,
  trTotals   :: Maybe v }
  deriving (Eq, Show)

sumTurnovers calcTotals ag list = do
  let c = agCurrency ag
  list' <- forM list $ \tr -> do
                cr' :# _ <- convert c (trCredit tr)
                dt' :# _ <- convert c (trDebit tr)
                inc' :# _ <- convert c (trIncSaldo tr)
                out' :# _ <- convert c (trOutSaldo tr)
                t'  <- case trTotals tr of
                         Nothing -> return Nothing
                         Just t -> do
                                   x :# _ <- convert c t
                                   return (Just x)
                return $ TRecord {
                           trCredit = cr',
                           trDebit  = dt',
                           trIncSaldo = inc',
                           trOutSaldo = out',
                           trTotals = t' }
  let credits = sum $ map trCredit list'
      debits  = sum $ map trDebit  list'
      inc     = sum $ map trIncSaldo list'
      out     = sum $ map trOutSaldo list'
      totals  = if calcTotals
                  then Just $ (sum $ map (fromJust . trTotals) list') :# c
                  else Nothing
  return $ TRecord {
             trCredit = credits :# c,
             trDebit  = debits  :# c,
             trIncSaldo = inc :# c,
             trOutSaldo = out :# c,
             trTotals = totals }

allTurnovers calcTotals qry account = do
  cr :# c <- creditTurnovers qry account
  dt :# _ <- debitTurnovers  qry account
  incomingSaldo <-
      case qStart qry of
        Nothing -> return (0 :# c)
        Just date -> do
            let startQry = qry {
                   qStart = Nothing,
                   qEnd   = Just date }
            saldo startQry account
  let outgoingSaldo = cr - dt
  return $ TRecord {
             trCredit = cr :# c,
             trDebit  = dt :# c,
             trIncSaldo = incomingSaldo,
             trOutSaldo = outgoingSaldo :# c,
             trTotals = if calcTotals
                          then Just $ (cr + dt) :# c
                          else Nothing }

noZeroTurns tr =
  case trTotals tr of
    Nothing -> isNotZero (trCredit tr) || isNotZero (trDebit tr)
    Just t  -> isNotZero t

turnovers' qry options mbPath = do
    coa <- case mbPath of
              Nothing   -> gets lsCoA
              Just path -> getCoAItem (gets lsPosition) (gets lsCoA) path
    let calcTotals = TShowTotals `elem` options
    tree <- mapTreeM (sumTurnovers calcTotals) (allTurnovers calcTotals qry) coa
    let tree' = if TNoZeros `elem` options
                  then filterLeafs noZeroTurns tree
                  else tree
    let struct = showTreeStructure tree'
        nodes = allNodes tree'
        credits = map trCredit nodes
        debits  = map trDebit  nodes
        inc     = map trIncSaldo nodes
        out     = map trOutSaldo nodes
        totals  = map trTotals nodes
    wrapIO $ putStrLn $ unlines $
      columns $
        [("ACCOUNT",     ALeft, struct),
         ("BALANCE C/F", ARight, map show inc),
         ("CREDIT",      ARight, map show credits),
         ("DEBIT",       ARight, map show debits),
         ("BALANCE B/D", ARight,  map show out)] ++
        if calcTotals
          then [("TOTALS", ARight, map (show . fromJust) totals)]
          else []
                  
