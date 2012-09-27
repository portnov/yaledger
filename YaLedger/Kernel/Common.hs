{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
-- | This module contains kernel functions, which do not
-- require Ledger monad.
module YaLedger.Kernel.Common where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Failure

import YaLedger.Types
import YaLedger.Exceptions

import Debug.Trace

-- | Filter list of CoA items by account type
filterByAccountType :: AccountGroupType -> [ChartOfAccounts] -> [ChartOfAccounts]
filterByAccountType t coas = filter (check t) coas
  where
    check AGCredit (Leaf {leafData = WCredit _ _}) = True
    check AGCredit (Leaf {leafData = WFree   _ _}) = True
    check AGDebit  (Leaf {leafData = WDebit  _ _}) = True
    check AGDebit  (Leaf {leafData = WFree   _ _}) = True
    check AGFree   (Leaf {leafData = WFree   _ _}) = True
    check t b@(Branch {}) = agType (branchData b) == t
    check _ _ = False

getCoAItem :: (Monad m,
               Failure InvalidPath m)
           => (m SourcePos)
           -> (m ChartOfAccounts)
           -> Path
           -> m ChartOfAccounts
getCoAItem getPos fn path = do
  coa <- fn
  pos <- getPos
  case search' coa path of
    [] -> failure (InvalidPath path [] pos)
    [a] -> return a
    as -> failure (InvalidPath path as pos)

getCoAItemT :: (Monad m,
                Failure InvalidPath m)
            => AccountGroupType
            -> (m SourcePos)
            -> (m ChartOfAccounts)
            -> Path
            -> m ChartOfAccounts
getCoAItemT t getPos fn path = do
  coa <- fn
  pos <- getPos
  case search' coa path of
    [] -> failure (InvalidPath path [] pos)
    [a] -> return a
    as -> case filterByAccountType t as of
            []  -> failure (InvalidPath path [] pos)
            [a] -> return a
            as  -> failure (InvalidPath path as pos)

getAccount :: (Monad m,
               Failure InvalidPath m,
               Failure NotAnAccount m)
           => (m SourcePos)
           -> (m ChartOfAccounts)
           -> Path
           -> m AnyAccount
getAccount getPos fn path = do
  x <- getCoAItem getPos fn path
  pos <- getPos
  case x of
    Leaf {} -> return (leafData x)
    _ -> failure (NotAnAccount path pos)

getAccountT :: (Monad m,
               Failure InvalidPath m,
               Failure NotAnAccount m)
           => AccountGroupType
           -> (m SourcePos)
           -> (m ChartOfAccounts)
           -> Path
           -> m AnyAccount
getAccountT t getPos fn path = do
  x <- getCoAItemT t getPos fn path
  pos <- getPos
  case x of
    Leaf {} -> return (leafData x)
    _ -> failure (NotAnAccount path pos)

