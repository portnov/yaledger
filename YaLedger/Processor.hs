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

putCreditPosting :: (Throws InvalidAccountType l)
               => Ext (Posting Decimal Credit)
               -> AnyAccount
               -> Ledger l AnyAccount
putCreditPosting e acc =
  case acc of
    WFree attrs account ->
      return $ WFree attrs $ credit account e
    WCredit attrs account ->
      return $ WCredit attrs $ credit account e
    WDebit _ _ ->
      throw (InvalidAccountType AGDebit AGCredit)

putDebitPosting :: (Throws InvalidAccountType l)
               => Ext (Posting Decimal Debit)
               -> AnyAccount
               -> Ledger l AnyAccount
putDebitPosting e acc = do
  case acc of
    WFree attrs account ->
      return $ WFree attrs $ debit account e
    WDebit attrs account ->
      return $ WDebit attrs $ debit account e
    WCredit _ _ ->
      throw (InvalidAccountType AGCredit AGDebit)

processEntry :: (Throws NoSuchRate l,
                   Throws NoCorrespondingAccountFound l,
                   Throws InvalidAccountType l)
               => DateTime
               -> Attributes
               -> Entry Decimal Unchecked
               -> Ledger l ()
processEntry date attrs uposting = do
  CEntry dt cr <- checkEntry attrs uposting
  forM dt $ \e -> updatePlan $ \plan ->
      updateAccount (getID $ debitPostingAccount e) plan (putDebitPosting $ Ext date attrs e)
  forM cr $ \e -> updatePlan $ \plan ->
      updateAccount (getID $ creditPostingAccount e) plan (putCreditPosting $ Ext date attrs e)
  return ()

processTransaction :: (Throws NoSuchRate l,
                       Throws NoCorrespondingAccountFound l,
                       Throws InvalidAccountType l)
                   => Ext Record
                   -> Ledger l ()
processTransaction (Ext date attrs (Transaction (TEntry p))) = do
  processEntry date attrs p
processTransaction x = fail $ show x

