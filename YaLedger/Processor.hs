{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, TemplateHaskell, NoMonomorphismRestriction #-}

module YaLedger.Processor
  (processRecords) where

import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Data.List
import Data.Decimal
import Data.Dates
import qualified Data.Map as M

import YaLedger.Types
import YaLedger.Types.Monad
import YaLedger.Types.Monad.STM
import YaLedger.Exceptions
import YaLedger.Kernel
import YaLedger.Kernel.Holds
import YaLedger.Logger
import qualified YaLedger.Logger.STM as STMLOG
import qualified YaLedger.Logger.Loggers as L
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

throwException :: Posting Decimal t -> Bool
throwException (CPosting {..}) = creditPostingUseHold == UseHold
throwException (DPosting {..}) = debitPostingUseHold  == UseHold

-- | Process one (unchecked yet) entry.
processEntry :: forall l.
                (Throws NoSuchRate l,
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
  $debug $ "Processing entry (" ++ show date ++ "):\n" ++ prettyPrint uentry
  queue <- gets lsTranQueue

  let useHoldIfNeeded :: HoldOperations t => HoldUsage -> Posting Decimal t -> Atomic l ()
      useHoldIfNeeded usage p
        | usage == DontUseHold = return ()
        | otherwise = closeHold date (Just entry) (>=) M.empty p
                       `catchWithSrcLoc` handleNoSuchHold (usage /= UseHold) WARNING

  -- All postings are done in single STM transaction.
  runAtomically $ do
      -- Process debit postings
      forM dt $ \p -> do
          let holdUsage = debitPostingUseHold p
          useHoldIfNeeded holdUsage p
          let account = debitPostingAccount p
          debit account entry (Ext date 0 pos attrs p)
          -- Add link to this entry for last balance (caused by previous call of `debit')
          -- modifyLastItem (\b -> b {causedBy = Just entry}) (accountBalances account)
          -- Run all needed rules
          runRules EDebit date tranID attrs p $ \tranID tran -> stm (enqueue tranID tran queue)

      -- Process credit postings
      forM cr $ \p -> do
          let holdUsage = creditPostingUseHold p
          useHoldIfNeeded holdUsage p
          let account = creditPostingAccount p
          credit account entry (Ext date 0 pos attrs p)
          -- Add link to this entry for last balance (caused by previous call of `credit')
          -- modifyLastItem (\b -> b {causedBy = Just entry}) (accountBalances account)
          -- Run all needed rules
          runRules ECredit date tranID attrs p $ \tranID tran -> stm (enqueue tranID tran queue)

      -- What to do with rates difference?
      case rd of
        OneCurrency -> return () -- There is no any difference
        CreditDifference ps -> forM_ ps $ \p -> do
            let holdUsage = creditPostingUseHold p
            useHoldIfNeeded holdUsage p
            let account = creditPostingAccount p
            credit (creditPostingAccount p) entry (Ext date 0 pos attrs p)
            -- modifyLastItem (\b -> b {causedBy = Just entry}) (accountBalances account)
            runRules ECredit date tranID attrs p $ \tranID tran -> stm (enqueue tranID tran queue)
        DebitDifference  ps -> forM_ ps $ \p -> do
              let account = debitPostingAccount p
              let holdUsage = debitPostingUseHold p
              useHoldIfNeeded holdUsage p
              debit  (debitPostingAccount  p) entry (Ext date 0 pos attrs p)
              -- modifyLastItem (\b -> b {causedBy = Just entry}) (accountBalances account)
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
  lift $ L.debug "YaLedger.Processor" $ "Record " ++ show (extID r) ++ ":\n" ++ prettyPrint r ++ "  at " ++ show pos

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
      lift $ $debug $ "Setting exchange rates:\n" ++ unlines (map show rates)
      let rates' = map (Ext date 0 pos attrs) rates
      lift $ modify $ \st -> st {lsRates = rates' ++ lsRates st}
      return []

    Just rec@(Ext _ _ pos _ _) -> do
      lift $ setPos pos
      lift $ $warning $ "Unknown record:\n" ++ prettyPrint rec ++ "\n  at " ++ show pos
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

-- | Set extID as [1..].
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

-- | Process all transactions from transactions queue.
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
                 chan <- gets lsMessages
                 wrapIO $ outputMessages chan
                 processTransactionsFromQueue

-- | Process one transaction
processTransaction :: forall l. (Throws NoSuchRate l,
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
processTransaction tranID (Ext date _ pos attrs (TReconciliate btype acc x tgt msg)) = do
    setPos pos
    mbEntry <- reconciliate btype date acc x tgt msg
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

processTransaction tranID (Ext date _ pos attrs (TCloseHolds crholds drholds)) = do
    setPos pos
    forM_ crholds $ goCloseHold
    forM_ drholds $ goCloseHold
  where
    goCloseHold :: HoldOperations t => CloseHold Amount t -> Ledger l ()
    goCloseHold clh = do
      let p = holdPosting $ holdToClose clh
          op = if searchLesserAmount clh
                 then (<=)
                 else (==)
          qry = searchAttributes clh
      p' <- convertPosting' (Just date) p
      runAtomically $ (closeHold date Nothing op qry p')
                        `catchWithSrcLoc` handleNoSuchHold (searchLesserAmount clh) INFO

handleNoSuchHold :: (Throws InternalError l,
                     Throws NoSuchHold l)
                 => Bool
                 -> Priority
                 -> CallTrace
                 -> NoSuchHold
                 -> Atomic l ()
handleNoSuchHold True  s _ e = STMLOG.logSTM "YaLedger.Processor" s $ show s ++ ": YaLedger.Processor: " ++ show e
handleNoSuchHold False _ l e = rethrow l e

