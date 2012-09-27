{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}

module YaLedger.Kernel.Common where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Failure

import YaLedger.Types
import YaLedger.Exceptions

import Debug.Trace

filterByAccountType :: AccountGroupType -> [AccountPlan] -> [AccountPlan]
filterByAccountType t plans = filter (check t) plans
  where
    check AGCredit (Leaf {leafData = WCredit _ _}) = True
    check AGCredit (Leaf {leafData = WFree   _ _}) = True
    check AGDebit  (Leaf {leafData = WDebit  _ _}) = True
    check AGDebit  (Leaf {leafData = WFree   _ _}) = True
    check AGFree   (Leaf {leafData = WFree   _ _}) = True
    check t b@(Branch {}) = agType (branchData b) == t
    check _ _ = False

getAccountPlanItem :: (Monad m,
                       Failure InvalidPath m)
                   => (m SourcePos)
                   -> (m AccountPlan)
                   -> Path
                   -> m AccountPlan
getAccountPlanItem getPos fn path = do
  plan <- fn
  pos <- getPos
  case search' plan path of
    [] -> failure (InvalidPath path [] pos)
    [a] -> return a
    as -> failure (InvalidPath path as pos)

getAccountPlanItemT :: (Monad m,
                       Failure InvalidPath m)
                   => AccountGroupType
                   -> (m SourcePos)
                   -> (m AccountPlan)
                   -> Path
                   -> m AccountPlan
getAccountPlanItemT t getPos fn path = do
  plan <- fn
  pos <- getPos
  case search' plan path of
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
           -> (m AccountPlan)
           -> Path
           -> m AnyAccount
getAccount getPos fn path = do
  x <- getAccountPlanItem getPos fn path
  pos <- getPos
  case x of
    Leaf {} -> return (leafData x)
    _ -> failure (NotAnAccount path pos)

getAccountT :: (Monad m,
               Failure InvalidPath m,
               Failure NotAnAccount m)
           => AccountGroupType
           -> (m SourcePos)
           -> (m AccountPlan)
           -> Path
           -> m AnyAccount
getAccountT t getPos fn path = do
  x <- getAccountPlanItemT t getPos fn path
  pos <- getPos
  case x of
    Leaf {} -> return (leafData x)
    _ -> failure (NotAnAccount path pos)


