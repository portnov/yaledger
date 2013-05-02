{-# LANGUAGE CPP, MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TemplateHaskell #-}
-- | 'Ledger' monad and utility functions.
--
-- Ledger monad is basically EMT l (StateT 'LedgerState' IO) a.
--
module YaLedger.Kernel.Monad where

import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Exception.Throws
import Control.Concurrent.STM
import Data.Dates
import qualified Data.Map as M
import Text.Parsec.Pos
import System.IO (stdout)
import System.Log.Logger (logM)

import YaLedger.Tree
import YaLedger.Types.Common
import YaLedger.Types.Ledger
import YaLedger.Types.Map
import YaLedger.Types.Transactions
import YaLedger.Types.Config
import YaLedger.Types.Monad
import YaLedger.Exceptions
import YaLedger.Exceptions.Utils
import YaLedger.Kernel.STM
import YaLedger.Processor.Queue
import YaLedger.Logger
import YaLedger.Types.Output

-- | Create default LedgerState.
emptyLedgerState :: LedgerOptions -> ChartOfAccounts -> AccountMap -> [Ext Record] -> IO LedgerState
emptyLedgerState opts coa amap records = do
  now <- getCurrentDateTime
  chan <- newTChanIO
  queue <- newQueue
  return $ LedgerState {
             lsStartDate = now,
             lsDefaultCurrency = emptyCurrency,
             lsCoA = coa,
             lsAccountMap = amap,
             lsFullGroupsMap = M.empty,
             lsTemplates = M.empty,
             lsRules = Rules [] [],
             lsRates = [],
             lsLoadedRecords = records,
             lsTranQueue = queue,
             lsMessages = chan,
             lsConfig = opts,
             lsOutput = stdout,
             lsOutputFormat = defaultOutputFormat,
             lsPosition = nowhere
           }

-- | Set current source location
setPos :: (Monad m) => SourcePos -> LedgerT l m ()
setPos pos =
  modify $ \st -> st {lsPosition = pos}

-- | Output all messages from 'lsMessages' queue.
outputMessages :: TChan (String, Priority, String) -> IO ()
outputMessages chan = do
  x <- atomically $ tryReadTChan chan
  case x of
    Nothing -> return ()
    Just (name, priority, message) -> do
        logM name priority message
        outputMessages chan

-- | Run 'Atomic' transaction.
-- This is similar to 'Control.Concurrent.STM.atomically'.
-- Any exception thrown within 'Atomic' monad will be rethrown
-- in 'Ledger' monad.
runAtomically :: Throws InternalError l => Atomic l a -> Ledger l a
runAtomically (EMT (LedgerStateT action)) = do -- `do' in StateT IO
    st <- get
    (result, st') <- wrapIO $ atomically $ runStateT action st
    case result of
      Right a -> do
                 put st'
                 return a
      Left (callTrace, e) -> wrapE $ rethrow callTrace (checkedException e)

runLedger :: MonadIO m => LedgerOptions -> ChartOfAccounts -> AccountMap -> [Ext Record] -> LedgerStateT m a -> m a
runLedger opts coa amap records action = do
  let LedgerStateT emt = action
  st <- liftIO $ emptyLedgerState opts coa amap records
  -- Use currency of root accounts group as default currency
  (res, _) <- runStateT emt (st {lsDefaultCurrency = agCurrency $ branchData coa})
  return res

-- * IOList
--
newIOList :: (MonadIO m, Throws InternalError l) => EMT l m (IOList a)
newIOList = wrapIO $ newTVarIO []

readIOList :: (Throws InternalError l) => IOList a -> Atomic l [a]
readIOList iolist = stm (readTVar iolist)

readIOListL :: Throws InternalError l => IOList a -> Ledger l [a]
readIOListL iolist = wrapIO $ atomically $ readTVar iolist

writeIOList :: (MonadIO m, Throws InternalError l) => IOList a -> [a] -> EMT l m ()
writeIOList iolist x = wrapIO (atomically $ writeTVar iolist x)

appendIOList :: (Throws InternalError l) => IOList a -> a -> Atomic l ()
appendIOList iolist x =
  stm $ modifyTVar iolist (x:)

-- | Update last item of 'IOList'
plusIOList :: (Throws InternalError l, Show a)
           => a          -- ^ Default value (put it to list if it's empty)
           -> (a -> Bool)
           -> (a -> a)
           -> IOList a
           -> Atomic l ()
plusIOList def p fn iolist = do
  stm $ modifyTVar iolist $ \list ->
    case filter p list of
      [] -> [def]
      (x:xs) -> fn x: x: xs
  res <- stm $ readTVar iolist
  $debugSTM $ "plusIOList: new item: " ++ show (head res)

modifyLastItem ::  (Throws InternalError l)
               => (a -> a)
               -> IOList (Ext a)
               -> Atomic l ()
modifyLastItem fn iolist = do
  stm $ modifyTVar iolist $ \list ->
    case list of
      [] -> []
      (x:xs) -> (x {getContent = fn (getContent x)}): xs

