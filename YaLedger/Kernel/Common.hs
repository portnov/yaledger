{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}

module YaLedger.Kernel.Common where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Failure

import YaLedger.Types
import YaLedger.Exceptions

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


