{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Processor where

import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Data.Dates
import Data.Decimal

import YaLedger.Types
import YaLedger.Monad
import YaLedger.Exceptions
import YaLedger.Correspondence
import YaLedger.Kernel

putCreditEntry :: (Throws InvalidAccountType l)
               => Ext (Entry Decimal Credit)
               -> AnyAccount
               -> Ledger l AnyAccount
putCreditEntry e acc =
  case acc of
    WFree attrs account ->
      return $ WFree attrs $ credit account e
    WCredit attrs account ->
      return $ WCredit attrs $ credit account e
    WDebit _ _ ->
      throw (InvalidAccountType AGDebit AGCredit)

putDebitEntry :: (Throws InvalidAccountType l)
               => Ext (Entry Decimal Debit)
               -> AnyAccount
               -> Ledger l AnyAccount
putDebitEntry e acc = do
  case acc of
    WFree attrs account ->
      return $ WFree attrs $ debit account e
    WDebit attrs account ->
      return $ WDebit attrs $ debit account e
    WCredit _ _ ->
      throw (InvalidAccountType AGCredit AGDebit)

processPosting :: (Throws NoSuchRate l,
                   Throws NoCorrespondingAccountFound l,
                   Throws InvalidAccountType l)
               => DateTime
               -> Attributes
               -> Posting Decimal Unchecked
               -> Ledger l ()
processPosting date attrs uposting = do
  CPosting dt cr <- checkPosting attrs uposting
  forM dt $ \e -> updatePlan $ \plan ->
      updateAccount (getID $ debitEntryAccount e) plan (putDebitEntry $ Ext date attrs e)
  forM cr $ \e -> updatePlan $ \plan ->
      updateAccount (getID $ creditEntryAccount e) plan (putCreditEntry $ Ext date attrs e)
  return ()

processTransaction :: (Throws NoSuchRate l,
                       Throws NoCorrespondingAccountFound l,
                       Throws InvalidAccountType l)
                   => Ext Record
                   -> Ledger l ()
processTransaction (Ext date attrs (Transaction (TPosting p))) = do
  processPosting date attrs p

