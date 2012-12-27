{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies, RecordWildCards #-}

module YaLedger.Reports.Holds
  (Holds (..)) where

import Control.Concurrent.STM

import YaLedger.Reports.API

data Holds = Holds

instance ReportClass Holds where
  type Options Holds = ()
  type Parameters Holds = Path

  runReport _ qry _ path = showHolds qry path

  reportHelp _ = "Show all holds for the account."

showHolds qry path = do
      coa <- getCoAItem (gets lsPosition) (gets lsCoA) path
      case coa of
        Leaf {..} -> showHolds' qry leafData
        _ -> throwP $ NotAnAccount path
  `catchWithSrcLoc`
    (\l (e :: NotAnAccount) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: InvalidPath) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: NoSuchRate) -> handler l e)

showHolds' qry account = do
  e1 <- wrapIO $ newTVarIO []
  e2 <- wrapIO $ newTVarIO []
  let creditHoldsHistory = case account of
                             WFree _ a -> freeAccountCreditHolds a
                             WCredit _ a -> creditAccountHolds a
                             WDebit _ a -> e1
      debitHoldsHistory  = case account of
                             WFree _ a -> freeAccountDebitHolds a
                             WCredit _ a -> e2
                             WDebit _ a -> debitAccountHolds a
  creditHolds <- runAtomically $ readIOList creditHoldsHistory
  debitHolds  <- runAtomically $ readIOList debitHoldsHistory

  wrapIO $ do
    forM_ (sort creditHolds) $ \hold ->
      putStrLn $ prettyPrint hold
    forM_ (sort debitHolds) $ \hold ->
      putStrLn $ prettyPrint hold

