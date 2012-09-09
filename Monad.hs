{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances #-}
module Monad where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Exception
import Data.Dates

import Types

newtype LedgerMonad a = LedgerMonad (State LedgerState a)
  deriving (Monad, MonadState LedgerState)

type Ledger l a = EMT l LedgerMonad a

data LedgerState = LedgerState {
  lsStartDate :: DateTime,
  lsDefaultCurrency :: Currency,
  lsAccountPlan :: AccountPlan,
  lsRates :: Rates }
  deriving (Eq, Show)

instance MonadState LedgerState (EMT l LedgerMonad) where
  get = lift get
  put s = lift (put s)

