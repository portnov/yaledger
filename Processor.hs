{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module Processor where

import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Data.Dates

import Types
import Monad
import Exceptions
import Correspondence
import Kernel

putCreditEntry :: (Throws InvalidAccountType l)
               => Ext (Entry Credit)
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
               => Ext (Entry Debit)
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
               -> Posting Unchecked
               -> Ledger l ()
processPosting date attrs uposting = do
  CPosting dt cr <- checkPosting attrs uposting
  forM dt $ \e -> updatePlan $ \plan ->
      updateAccount (getID $ debitEntryAccount e) plan (putDebitEntry $ Ext date attrs e)
  forM cr $ \e -> updatePlan $ \plan ->
      updateAccount (getID $ creditEntryAccount e) plan (putCreditEntry $ Ext date attrs e)
  return ()



