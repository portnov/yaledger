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
import YaLedger.Kernel.Holds
import YaLedger.Logger
import YaLedger.Processor.Duplicates
import YaLedger.Processor.Rules
import YaLedger.Processor.Templates
import YaLedger.Output.Pretty
import YaLedger.Kernel.Queue

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
                 Throws ReconciliationError l,
                 Throws NoSuchHold l,
                 Throws InternalError l)
               => DateTime                -- ^ Date/time of entry
               -> Integer
               -> SourcePos               -- ^ Location of entry in source file
               -> Attributes              -- ^ Entry attributes
               -> Entry Amount Unchecked  -- ^ Entry itself
               -> Ledger l ()
processEntry date tranID pos attrs uentry = do
  -- First, check the entry
  entry@(CEntry dt cr rd) <- checkEntry date attrs uentry
  queue <- gets lsTranQueue

  -- All postings are done in single STM transaction.
  runAtomically $ do
      -- Process debit postings
      forM dt $ \p -> do
          let account = debitPostingAccount p
          bal <- getCurrentBalance AvailableBalance account
          debugSTM $ show date ++ ": Balance before posting: " ++ show bal
          debit  account (Ext date 0 pos attrs p)
          -- Add link to this entry for last balance (caused by previous call of `debit')
          modifyLastItem (\b -> b {causedBy = Just entry}) (accountBalances account)
          -- Run all needed rules
          runRules EDebit date tranID attrs p $ \tranID tran -> stm (enqueue tranID tran queue)

      -- Process credit postings
      forM cr $ \p -> do
          let account = creditPostingAccount p
          credit account (Ext date 0 pos attrs p)
          -- Add link to this entry for last balance (caused by previous call of `credit')
          modifyLastItem (\b -> b {causedBy = Just entry}) (accountBalances account)
          -- Run all needed rules
          runRules ECredit date tranID attrs p $ \tranID tran -> stm (enqueue tranID tran queue)

      -- What to do with rates difference?
      case rd of
        OneCurrency -> return () -- There is no any difference
        CreditDifference p -> do
            let account = creditPostingAccount p
            credit (creditPostingAccount p) (Ext date 0 pos attrs p)
            modifyLastItem (\b -> b {causedBy = Just entry}) (accountBalances account)
            runRules ECredit date tranID attrs p $ \tranID tran -> stm (enqueue tranID tran queue)
        -- For debit difference, there might be many postings,
        -- caused by debit redirection
        DebitDifference  ps -> forM_ ps $ \ p -> do
              let account = debitPostingAccount p
              debit  (debitPostingAccount  p) (Ext date 0 pos attrs p)
              modifyLastItem (\b -> b {causedBy = Just entry}) (accountBalances account)
              runRules EDebit date tranID attrs p $ \tranID tran -> stm (enqueue tranID tran queue)
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

debugPos pos r =
  lift $ debug $ "Record:\n" ++ prettyPrint r ++ "  at " ++ show pos

insertRule new [] = [new]
insertRule new@(name, _, _) list = go list list
  where
    go f [] = new: f
    go f ((n,c,w):rs)
      | n == name = new: rs
      | otherwise = (n,c,w): go f rs

-- | Process one record.
processRecord :: (Throws InternalError l,
                  Throws InsufficientFunds l)
              => StateT [Ext Record] (EMT l (LedgerStateT IO)) [Ext (Transaction Amount)]
processRecord = do
  rec <- getNext
  case rec of
    Nothing -> return []

    Just rec@(Ext date i pos attrs (Transaction tran)) -> do
      debugPos pos rec
      return [ Ext date i pos attrs tran ]

    Just rec@(Ext _ _ pos attrs (Template name tran)) -> do
      debugPos pos rec
      lift $ modify $ \st -> st {lsTemplates = M.insert name (attrs, tran) (lsTemplates st)}
      return []

    Just rec@(Ext _ _ pos attrs (RuleR name cond tran)) -> do
      debugPos pos rec
      lift $ modify $ \st ->
                       let old = lsRules st
                           new = (name, attrs, When cond tran)
                       in case cAction cond of
                            Nothing -> st {lsRules = old {creditRules = insertRule new (creditRules old),
                                                          debitRules  = insertRule new (debitRules  old) } }
                            Just ECredit -> st {lsRules = old {creditRules = insertRule new (creditRules old) }}
                            Just EDebit  -> st {lsRules = old {debitRules  = insertRule new (debitRules  old) }}
      return []

    Just rec@(Ext date _ pos attrs (Periodic name interval tran)) -> do
      debugPos pos rec
      mbNext <- getNextP (periodic name . getContent)
                         (isStop name   . getContent)
      let prune = case getDate <$> mbNext of
                    Just dateX -> takeWhile (\x -> getDate x < dateX)
                    Nothing    -> id
      let listFrom start =
              Ext start 0 pos attrs tran: listFrom (start `addInterval` interval)
      let result = prune $ listFrom date
      -- lift $ debug $ "Periodic " ++ name ++ ": " ++ show (length result)
      return result

    Just rec@(Ext date _ pos attrs (SetRate rates)) -> do
      debugPos pos rec
      lift $ setPos pos
      lift $ debug $ "Setting exchange rates:\n" ++ unlines (map show rates)
      let rates' = map (Ext date 0 pos attrs) rates
      lift $ modify $ \st -> st {lsRates = rates' ++ lsRates st}
      return []

    Just rec@(Ext _ _ pos _ _) -> do
      lift $ setPos pos
      lift $ warning $ "Unknown record:\n" ++ prettyPrint rec ++ "\n  at " ++ show pos
      return []

periodic name (Periodic x _ _) = name == x
periodic name (StopPeriodic x) = name == x
periodic _    _                = False

isStop name (StopPeriodic x) = name == x
isStop _    _                = False

-- | Process all records.
processAll :: (Throws InternalError l,
               Throws InsufficientFunds l)
           => StateT [Ext Record] (EMT l (LedgerStateT IO)) [Ext (Transaction Amount)]
processAll = do
  trans <- processRecord
  finish <- gets null
  if finish
    then return trans
    else do
         other <- processAll
         return $ merge other trans

enumerate :: [Ext a] -> [Ext a]
enumerate list = zipWith go [1..] list
  where
    go i e = e {extID = i}

-- | Process all records.
processRecords :: (Throws NoSuchRate l,
                   Throws NoCorrespondingAccountFound l,
                   Throws InvalidAccountType l,
                   Throws NoSuchTemplate l,
                   Throws InsufficientFunds l,
                   Throws ReconciliationError l,
                   Throws DuplicatedRecord l,
                   Throws NoSuchHold l,
                   Throws InternalError l)
               => DateTime                   -- ^ Process records with date <= this
               -> [DeduplicationRule]        -- ^ Rules to deduplicate records
               -> [Ext Record]               -- ^ All records
               -> Ledger l ()
processRecords endDate rules list = do
  deduplicated <- deduplicate rules (enumerate $ sort list)
  let records = sort deduplicated
  modify $ \st -> st {lsLoadedRecords = records}
  list' <- evalStateT processAll records
  queue <- gets lsTranQueue
  let ns = [1, 100 ..]
  runAtomically $
    forM_ (zip ns (takeWhile (\t -> getDate t <= endDate) list')) $ \(tranID, tran) ->
        stm $ enqueue tranID tran queue
  processTransactionsFromQueue 

processTransactionsFromQueue :: (Throws NoSuchRate l,
                       Throws NoCorrespondingAccountFound l,
                       Throws InvalidAccountType l,
                       Throws NoSuchTemplate l,
                       Throws InsufficientFunds l,
                       Throws ReconciliationError l,
                       Throws NoSuchHold l,
                       Throws InternalError l)
                  => Ledger l ()
processTransactionsFromQueue = do
  queue <- gets lsTranQueue
  mbTran <- runAtomically $ stm $ getFromQueue queue
  case mbTran of
    -- If queue is empty, then nothing to do.
    Nothing -> return ()
    Just (tranID, tran) -> do
                 processTransaction tranID tran
                 processTransactionsFromQueue

-- | Process one transaction
processTransaction :: (Throws NoSuchRate l,
                       Throws NoCorrespondingAccountFound l,
                       Throws InvalidAccountType l,
                       Throws NoSuchTemplate l,
                       Throws InsufficientFunds l,
                       Throws ReconciliationError l,
                       Throws NoSuchHold l,
                       Throws InternalError l)
                   => Integer
                   -> Ext (Transaction Amount)
                   -> Ledger l ()
processTransaction tranID (Ext date _ pos attrs (TEntry p)) = do
    setPos pos
    processEntry date tranID pos attrs p
processTransaction tranID (Ext date _ pos attrs (TReconciliate acc x tgt msg)) = do
    setPos pos
    mbEntry <- reconciliate date acc x tgt msg
    case mbEntry of
      Nothing -> return ()
      Just entry -> processEntry date tranID pos
                                 (M.insert "category" (Exactly "reconciliation") attrs)
                                 entry

processTransaction tranID (Ext date _ pos attrs (TCallTemplate name args)) = do
    setPos pos
    (tplAttrs, template) <- getTemplate name
    tran <- fillTemplate template args
    processTransaction tranID (Ext date 0 pos (attrs `M.union` tplAttrs) tran)

processTransaction tranID (Ext date _ pos attrs (THold crholds dtholds)) = do
    setPos pos

    forM_ crholds $ \(Hold posting mbEnd) -> do
      p <- convertPosting' (Just date) posting
      runAtomically $ creditHold (postingAccount' p) $ Ext date 0 pos attrs (Hold p mbEnd)

    forM_ dtholds $ \(Hold posting mbEnd) -> do
      p <- convertPosting' (Just date) posting
      runAtomically $ debitHold (postingAccount' p) $ Ext date 0 pos attrs (Hold p mbEnd)

processTransaction tranID (Ext date _ pos attrs (TCloseCreditHold (Hold p@(CPosting acc amt) _))) = do
    setPos pos
    p' <- convertPosting' (Just date) p
    runAtomically $ closeHold date p'

processTransaction tranID (Ext date _ pos attrs (TCloseDebitHold (Hold p@(DPosting acc amt) _))) = do
    setPos pos
    p' <- convertPosting' (Just date) p
    runAtomically $ closeHold date p'

