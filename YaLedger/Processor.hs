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
import YaLedger.Rules

processEntry :: (Throws NoSuchRate l,
                 Throws NoCorrespondingAccountFound l,
                 Throws InvalidAccountType l,
                 Throws NoSuchTemplate l,
                 Throws InternalError l)
               => DateTime
               -> Attributes
               -> Entry Amount Unchecked
               -> Ledger l ()
processEntry date attrs uentry = do
  CEntry dt cr rd <- checkEntry attrs uentry
  message $ "Entry:\n" ++ show (CEntry dt cr rd)
  forM dt $ \p -> do
      debit  (debitPostingAccount  p) (Ext date attrs p)
      runRules date attrs p processTransaction
  forM cr $ \p -> do
      credit (creditPostingAccount p) (Ext date attrs p)
      runRules date attrs p processTransaction
  case rd of
    OneCurrency -> return ()
    CreditDifference p -> do
        credit (creditPostingAccount p) (Ext date attrs p)
        runRules date attrs p processTransaction
    DebitDifference  p -> do
        debit  (debitPostingAccount  p) (Ext date attrs p)
        runRules date attrs p processTransaction
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
processTransaction (Ext _ attrs (Template name tran)) = do
    modify $ \st -> st {lsTemplates = M.insert name (attrs, tran) (lsTemplates st)}
processTransaction (Ext _ attrs (RuleR name cond tran)) = do
    modify $ \st -> st {lsRules = (name, attrs, When cond tran):lsRules st}
processTransaction (Ext date attrs (Transaction (TCallTemplate name args))) = do
    (tplAttrs, template) <- getTemplate name
    tran <- fillTemplate template args
    processTransaction (Ext date (attrs `M.union` tplAttrs) (Transaction tran))
processTransaction (Ext date attrs (Transaction (TReconciliate acc x))) = do
    entry <- reconciliate date acc x
    processEntry date (M.insert "category" (Exactly "reconciliation") attrs) entry
processTransaction (Ext _ _ (Transaction (TSetRate c1 c2 x))) = do
    modify $ \st -> st {lsRates = M.insert (c1, c2) x (lsRates st)}
processTransaction x = fail $ show x

