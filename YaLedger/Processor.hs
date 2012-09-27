{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
{- # OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Processor where

import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Data.List
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

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) =
  if x < y
    then x: merge xs (y:ys)
    else y: merge (x:xs) ys

mergeAll :: Ord a => [[a]] -> [a]
mergeAll = foldr1 merge

processEntry :: (Throws NoSuchRate l,
                 Throws NoCorrespondingAccountFound l,
                 Throws InvalidAccountType l,
                 Throws NoSuchTemplate l,
                 Throws InternalError l)
               => DateTime
               -> SourcePos
               -> Attributes
               -> Entry Amount Unchecked
               -> Ledger l ()
processEntry date pos attrs uentry = do
  entry@(CEntry dt cr rd) <- checkEntry attrs uentry
  message $ show date ++ ":\n" ++ show entry
  message $ showA attrs
  forM dt $ \p -> do
      let account = debitPostingAccount p
      debit  account (Ext date pos attrs p)
      modifyLastItem (\b -> b {causedBy = Just entry}) (accountBalances account)
      runRules date attrs p processTransaction
  forM cr $ \p -> do
      let account = creditPostingAccount p
      credit account (Ext date pos attrs p)
      modifyLastItem (\b -> b {causedBy = Just entry}) (accountBalances account)
      runRules date attrs p processTransaction
  case rd of
    OneCurrency -> return ()
    CreditDifference p -> do
        let account = creditPostingAccount p
        credit (creditPostingAccount p) (Ext date pos attrs p)
        modifyLastItem (\b -> b {causedBy = Just entry}) (accountBalances account)
        runRules date attrs p processTransaction
    DebitDifference  p -> do
        let account = debitPostingAccount p
        debit  (debitPostingAccount  p) (Ext date pos attrs p)
        modifyLastItem (\b -> b {causedBy = Just entry}) (accountBalances account)
        runRules date attrs p processTransaction
  return ()

getNext :: Monad m => StateT [a] m (Maybe a)
getNext = do
  list <- get
  case list of
    [] -> return Nothing
    (x:xs) -> do
              put xs
              return (Just x)

getNextP :: (Monad m, Eq a) => (a -> Bool) -> (a -> Bool) -> StateT [a] m (Maybe a)
getNextP p doDelete = do
  list <- get
  case filter p list of
    [] -> return Nothing
    (x:xs) -> do
        if doDelete x
          then put (delete x list)
          else return ()
        return (Just x)

processRecord :: Throws InternalError l
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
      lift $ modify $ \st -> st {lsRules = (name, attrs, When cond tran):lsRules st}
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
      lift $ message $ "Periodic " ++ name ++ ": " ++ show (length result)
      return result

periodic name (Periodic x _ _) = name == x
periodic name (StopPeriodic x) = name == x
periodic _    _                = False

isStop name (StopPeriodic x) = name == x
isStop _    _                = False

processAll :: Throws InternalError l
           => StateT [Ext Record] (EMT l LedgerMonad) [Ext (Transaction Amount)]
processAll = do
  trans <- processRecord
  finish <- gets null
  if finish
    then return trans
    else do
         other <- processAll
         return $ merge other trans

processRecords :: (Throws NoSuchRate l,
                   Throws NoCorrespondingAccountFound l,
                   Throws InvalidAccountType l,
                   Throws NoSuchTemplate l,
                   Throws InternalError l)
               => [Ext Record]
               -> Ledger l ()
processRecords list = do
  list' <- evalStateT processAll (sort list)
  now <- wrapIO getCurrentDateTime 
  forM_ (takeWhile (\t -> getDate t <= now) list') $
      processTransaction

processTransaction :: (Throws NoSuchRate l,
                       Throws NoCorrespondingAccountFound l,
                       Throws InvalidAccountType l,
                       Throws NoSuchTemplate l,
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
processTransaction (Ext _ pos _ (TSetRate c1 c2 x)) = do
    setPos pos
    modify $ \st -> st {lsRates = M.insert (c1, c2) x (lsRates st)}
processTransaction (Ext date pos attrs (TCallTemplate name args)) = do
    setPos pos
    (tplAttrs, template) <- getTemplate name
    tran <- fillTemplate template args
    processTransaction (Ext date pos (attrs `M.union` tplAttrs) tran)
processTransaction x = fail $ show x

