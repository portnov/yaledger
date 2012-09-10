{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Monad where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc
import Data.Dates
import qualified Data.Map as M

import YaLedger.Types

newtype LedgerMonad a = LedgerMonad (StateT LedgerState IO a)
  deriving (Monad, MonadState LedgerState, MonadIO)

type Ledger l a = EMT l LedgerMonad a

data LedgerState = LedgerState {
  lsStartDate :: DateTime,
  lsDefaultCurrency :: Currency,
  lsAccountPlan :: AccountPlan,
  lsAccountMap :: AccountMap,
  lsTemplates :: M.Map String (Transaction Param),
  lsRates :: Rates }
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
             lsTemplates = M.empty,
             lsRates = M.empty }

message :: String -> Ledger l ()
message str =
  (liftIO $ putStrLn $ ">> " ++ str)
    `catchWithSrcLoc`
      \loc (e :: SomeException) -> fail (showExceptionWithTrace loc e)

runLedger :: AccountPlan -> AccountMap -> LedgerMonad a -> IO a
runLedger plan amap action = do
  let LedgerMonad emt = action
  st <- emptyLedgerState plan amap
  (res, _) <- runStateT emt st
  return res

