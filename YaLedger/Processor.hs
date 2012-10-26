{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}

module YaLedger.Processor
  (processRecords) where

import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Data.List
import Data.Dates
import qualified Data.Map as M

import YaLedger.Types
import YaLedger.Exceptions
import YaLedger.Kernel
import YaLedger.Logger
import YaLedger.Processor.Duplicates
import YaLedger.Processor.Rules
import YaLedger.Processor.Templates

-- | Merge two sorted lists into one sorted.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) =
  if x < y
    then x: merge xs (y:ys)
    else y: merge (x:xs) ys

-- | Process one (unchecked yet) entry.
processEntry :: (Throws NoSuchRate l,
                 Throws NoCorrespondingAccountFound l,
                 Throws InvalidAccountType l,
                 Throws NoSuchTemplate l,
                 Throws InsufficientFunds l,
                 Throws InternalError l)
               => DateTime                -- ^ Date/time of entry
               -> SourcePos               -- ^ Location of entry in source file
               -> Attributes              -- ^ Entry attributes
               -> Entry Amount Unchecked  -- ^ Entry itself
               -> Ledger l ()
processEntry date pos attrs uentry = do
  -- First, check the entry
  entry@(CEntry dt cr rd) <- checkEntry date attrs uentry

  -- Process debit postings
  forM dt $ \p -> do
      let account = debitPostingAccount p
      debit  account (Ext date pos attrs p)
      -- Add link to this entry for last balance (caused by previous call of `debit')
      modifyLastItem (\b -> b {causedBy = Just entry}) (accountBalances account)
      -- Run all needed rules
      runRules EDebit date attrs p processTransaction

  -- Process credit postings
  forM cr $ \p -> do
      let account = creditPostingAccount p
      credit account (Ext date pos attrs p)
      -- Add link to this entry for last balance (caused by previous call of `credit')
      modifyLastItem (\b -> b {causedBy = Just entry}) (accountBalances account)
      -- Run all needed rules
      runRules ECredit date attrs p processTransaction

  -- What to do with rates difference?
  case rd of
    OneCurrency -> return () -- There is no any difference
    CreditDifference p -> do
        let account = creditPostingAccount p
        credit (creditPostingAccount p) (Ext date pos attrs p)
        modifyLastItem (\b -> b {causedBy = Just entry}) (accountBalances account)
        runRules ECredit date attrs p processTransaction
    -- Dor debit difference, there might be many postings,
    -- caused by debit redirection
    DebitDifference  ps -> forM_ ps $ \ p -> do
          let account = debitPostingAccount p
          debit  (debitPostingAccount  p) (Ext date pos attrs p)
          modifyLastItem (\b -> b {causedBy = Just entry}) (accountBalances account)
          runRules EDebit date attrs p processTransaction
  return ()

-- | Get next record
getNext :: Monad m => StateT [a] m (Maybe a)
getNext = do
  list <- get
  case list of
    [] -> return Nothing
    (x:xs) -> do
              put xs
              return (Just x)

-- | Get next record, for which predicate is True
getNextP :: (Monad m, Eq a)
         => (a -> Bool)            -- ^ Is this record ok?
         -> (a -> Bool)            -- ^ Should we delete this record?
         -> StateT [a] m (Maybe a)
getNextP p doDelete = do
  list <- get
  case filter p list of
    [] -> return Nothing
    (x:xs) -> do
        if doDelete x
          then put (delete x list)
          else return ()
        return (Just x)

-- | Process one record.
processRecord :: (Throws InternalError l,
                  Throws InsufficientFunds l)
              => StateT [Ext Record] (EMT l LedgerMonad) [Ext (Transaction Amount)]
processRecord = do
  rec <- getNext
  case rec of
    Nothing -> return []

    Just (Ext date pos attrs (Transaction tran)) ->
      return [ Ext date pos attrs tran ]

    Just (Ext _ _ attrs (Template name tran)) -> do
      lift $ modify $ \st -> st {lsTemplates = M.insert name (attrs, tran) (lsTemplates st)}
      return []

    Just (Ext _ _ attrs (RuleR name cond tran)) -> do
      lift $ modify $ \st ->
                       let old = lsRules st
                           new = (name, attrs, When cond tran)
                       in case cAction cond of
                            Nothing -> st {lsRules = old {creditRules = new: creditRules old,
                                                          debitRules  = new: debitRules  old} }
                            Just ECredit -> st {lsRules = old {creditRules = new: creditRules old}}
                            Just EDebit  -> st {lsRules = old {debitRules  = new: debitRules  old}}
      return []

    Just (Ext date pos attrs (Periodic name interval tran)) -> do
      mbNext <- getNextP (periodic name . getContent)
                         (isStop name   . getContent)
      let prune = case getDate <$> mbNext of
                    Just dateX -> takeWhile (\x -> getDate x < dateX)
                    Nothing    -> id
      let listFrom start =
              Ext start pos attrs tran: listFrom (start `addInterval` interval)
      let result = prune $ listFrom date
      -- lift $ debug $ "Periodic " ++ name ++ ": " ++ show (length result)
      return result

    Just (Ext date pos attrs (SetRate rates)) -> do
      lift $ setPos pos
      lift $ debug $ "Setting exchange rates:\n" ++ unlines (map show rates)
      let rates' = map (Ext date pos attrs) rates
      lift $ modify $ \st -> st {lsRates = rates' ++ lsRates st}
      return []

periodic name (Periodic x _ _) = name == x
periodic name (StopPeriodic x) = name == x
periodic _    _                = False

isStop name (StopPeriodic x) = name == x
isStop _    _                = False

-- | Process all records.
processAll :: (Throws InternalError l,
               Throws InsufficientFunds l)
           => StateT [Ext Record] (EMT l LedgerMonad) [Ext (Transaction Amount)]
processAll = do
  trans <- processRecord
  finish <- gets null
  if finish
    then return trans
    else do
         other <- processAll
         return $ merge other trans

-- | Process all records.
processRecords :: (Throws NoSuchRate l,
                   Throws NoCorrespondingAccountFound l,
                   Throws InvalidAccountType l,
                   Throws NoSuchTemplate l,
                   Throws InsufficientFunds l,
                   Throws DuplicatedRecord l,
                   Throws InternalError l)
               => DateTime                   -- ^ Process records with date <= this
               -> [DeduplicationRule]        -- ^ Rules to deduplicate records
               -> [Ext Record]               -- ^ All records
               -> Ledger l ()
processRecords endDate rules list = do
  deduplicated <- deduplicate rules (sort list)
  let records = sort deduplicated
  modify $ \st -> st {lsLoadedRecords = records}
  list' <- evalStateT processAll records
  forM_ (takeWhile (\t -> getDate t <= endDate) list') $
      processTransaction

-- | Process one transaction
processTransaction :: (Throws NoSuchRate l,
                       Throws NoCorrespondingAccountFound l,
                       Throws InvalidAccountType l,
                       Throws NoSuchTemplate l,
                       Throws InsufficientFunds l,
                       Throws InternalError l)
                   => Ext (Transaction Amount)
                   -> Ledger l ()
processTransaction (Ext date pos attrs (TEntry p)) = do
    setPos pos
    processEntry date pos attrs p
processTransaction (Ext date pos attrs (TReconciliate acc x)) = do
    setPos pos
    entry <- reconciliate date acc x
    processEntry date
                 pos
                 (M.insert "category" (Exactly "reconciliation") attrs)
                 entry
processTransaction (Ext date pos attrs (TCallTemplate name args)) = do
    setPos pos
    (tplAttrs, template) <- getTemplate name
    tran <- fillTemplate template args
    processTransaction (Ext date pos (attrs `M.union` tplAttrs) tran)

