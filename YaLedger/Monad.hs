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
import Data.IORef

import YaLedger.Types
import YaLedger.Exceptions

newtype LedgerMonad a = LedgerMonad (StateT LedgerState IO a)
  deriving (Monad, MonadState LedgerState, MonadIO)

type Ledger l a = EMT l LedgerMonad a

data LedgerState = LedgerState {
  lsStartDate :: DateTime,
  lsDefaultCurrency :: Currency,
  lsAccountPlan :: AccountPlan,
  lsAccountMap :: AccountMap,
  lsTemplates :: M.Map String (Attributes, Transaction Param),
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

wrapIO :: (MonadIO m, Throws InternalError l)
       => IO a
       -> EMT l m a
wrapIO action = wrapE $ liftIO action

message :: Throws InternalError l => String -> Ledger l ()
message str =
  wrapIO $ putStrLn $ ">> " ++ str

runLedger :: AccountPlan -> AccountMap -> LedgerMonad a -> IO a
runLedger plan amap action = do
  let LedgerMonad emt = action
  st <- emptyLedgerState plan amap
  -- Use currency of root accounts group as default currency
  (res, _) <- runStateT emt (st {lsDefaultCurrency = agCurrency $ branchData plan})
  return res

-- * IOList
--
newIOList :: (MonadIO m, Throws InternalError l) => EMT l m (IOList a)
newIOList = wrapIO $ newIORef []

appendIOList :: (MonadIO m, Throws InternalError l) => IOList a -> a -> EMT l m ()
appendIOList iolist x = wrapIO $ modifyIORef iolist (x:)

readIOList :: (MonadIO m, Throws InternalError l) => IOList a -> EMT l m [a]
readIOList iolist = wrapIO (readIORef iolist)

