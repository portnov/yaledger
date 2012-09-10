{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances #-}
module YaLedger.Monad where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Data.Dates
import qualified Data.Map as M

import YaLedger.Types

newtype LedgerMonad a = LedgerMonad (State LedgerState a)
  deriving (Monad, MonadState LedgerState)

type Ledger l a = EMT l LedgerMonad a

data LedgerState = LedgerState {
  lsStartDate :: DateTime,
  lsDefaultCurrency :: Currency,
  lsAccountPlan :: AccountPlan,
  lsAccountMap :: AccountMap,
  lsRates :: Rates,
  lsMessages :: [String] }
  deriving (Eq, Show)

instance MonadState LedgerState (EMT l LedgerMonad) where
  get = lift get
  put s = lift (put s)

emptyLedgerState :: AccountPlan -> AccountMap -> IO LedgerState
emptyLedgerState plan amap = do
  now <- getCurrentDateTime
  return $ LedgerState {
             lsStartDate = now,
             lsDefaultCurrency = "",
             lsAccountPlan = plan,
             lsAccountMap = amap,
             lsRates = M.empty,
             lsMessages = [] }

message :: String -> Ledger l ()
message str =
  modify $ \st -> st {lsMessages = lsMessages st ++ [str]}

runLedger :: AccountPlan -> AccountMap -> LedgerMonad a -> IO (a, [String])
runLedger plan amap action = do
  let LedgerMonad emt = action
  st <- emptyLedgerState plan amap
  let (res, st') = runState emt st
  return (res, lsMessages st')

