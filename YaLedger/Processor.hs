{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Processor where

import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Data.Dates
import Data.Decimal
import qualified Data.Map as M

import YaLedger.Types
import YaLedger.Monad
import YaLedger.Exceptions
import YaLedger.Correspondence
import YaLedger.Kernel
import YaLedger.Templates

putCreditPosting :: (Throws InvalidAccountType l,
                     Throws InternalError l)
               => Ext (Posting Amount Credit)
               -> AnyAccount
               -> Ledger l ()
putCreditPosting p acc = do
  message $ "Credit posting: " ++ show p
  case acc of
    WFree   attrs account -> credit account p
    WCredit attrs account -> credit account p
    WDebit _ _ ->
      throw (InvalidAccountType AGDebit AGCredit)

putDebitPosting :: (Throws InvalidAccountType l,
                    Throws InternalError l)
               => Ext (Posting Amount Debit)
               -> AnyAccount
               -> Ledger l ()
putDebitPosting p acc = do
  case acc of
    WFree  attrs account -> debit account p
    WDebit attrs account -> debit account p
    WCredit _ _ ->
      throw (InvalidAccountType AGCredit AGDebit)

processEntry :: (Throws NoSuchRate l,
                 Throws NoCorrespondingAccountFound l,
                 Throws InvalidAccountType l,
                 Throws NoSuchTemplate l,
                 Throws InternalError l)
               => DateTime
               -> Attributes
               -> Entry Amount Unchecked
               -> Ledger l ()
processEntry date attrs uposting = do
  CEntry dt cr <- checkEntry attrs uposting
  forM dt $ \p -> updatePlan $ \plan ->
      updateAccount (debitPostingAccount p) plan (putDebitPosting $ Ext date attrs p)
  forM cr $ \e -> updatePlan $ \plan -> do
      message $ "Credit posting at acc. #" ++ show (creditPostingAccount e)
      res <- updateAccount (creditPostingAccount e) plan (putCreditPosting $ Ext date attrs e)
      return res
  return ()

processTransaction :: (Throws NoSuchRate l,
                       Throws NoCorrespondingAccountFound l,
                       Throws InvalidAccountType l,
                       Throws NoSuchTemplate l,
                       Throws InternalError l)
                   => Ext Record
                   -> Ledger l ()
processTransaction (Ext date attrs (Transaction (TEntry p))) = do
    processEntry date attrs p
processTransaction (Ext _ _ (Template name tran)) = do
    modify $ \st -> st {lsTemplates = M.insert name tran (lsTemplates st)}
processTransaction (Ext date attrs (Transaction (TCallTemplate name args))) = do
    template <- getTemplate name
    tran <- fillTemplate template args
    processTransaction (Ext date attrs (Transaction tran))
processTransaction (Ext date attrs (Transaction (TReconciliate acc x))) = do
    entry <- reconciliate date acc x
    processEntry date (("category", "reconciliation"):attrs) entry
processTransaction (Ext _ _ (Transaction (TSetRate c1 c2 x))) = do
    modify $ \st -> st {lsRates = M.insert (c1, c2) x (lsRates st)}
processTransaction x = fail $ show x

