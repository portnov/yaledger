{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}
-- | 'Ledger' monad and utility functions.
--
-- Ledger monad is basically EMT l (StateT 'LedgerState' IO) a.
--
module YaLedger.Types.Monad where

import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Exception.Throws
import Control.Monad.Loc
import Control.Concurrent.STM
import Data.Dates
import qualified Data.Map as M
import Text.Parsec.Pos
import System.Log.Logger (Priority (..), rootLoggerName, logM)

import YaLedger.Types.Attributes
import YaLedger.Types.Common
import YaLedger.Types.Ledger
import YaLedger.Types.Map
import YaLedger.Types.Transactions
import YaLedger.Types.Config
import YaLedger.Tree
import YaLedger.Exceptions
import YaLedger.Queue

newtype LedgerStateT m a = LedgerStateT (StateT LedgerState m a)
  deriving (Monad, MonadState LedgerState, MonadIO)

type LedgerT l m a = EMT l (LedgerStateT m) a

type Ledger l a = LedgerT l IO a

type LedgerSTM l a = LedgerT l STM a

stm2io :: Throws InternalError l => LedgerSTM l a -> Ledger l a
stm2io (EMT (LedgerStateT action)) = do -- `do' in StateT IO
    st <- get
    (result, st') <- wrapIO $ atomically $ runStateT action st
    case result of
      Right a -> do
                 put st'
                 return a
      Left (callTrace, e) -> wrapE $ rethrow callTrace (checkedException e)

data Rules = Rules {
    creditRules :: [(String, Attributes, Rule)]
  , debitRules  ::  [(String, Attributes, Rule)]
  }
  deriving (Eq, Show)

-- | Ledger state
data LedgerState = LedgerState {
    lsStartDate       :: DateTime,
    lsDefaultCurrency :: Currency,
    lsCoA             :: ChartOfAccounts,
    lsAccountMap      :: AccountMap,
    lsFullGroupsMap   :: M.Map AccountID [GroupID],
    lsTemplates       :: M.Map String (Attributes, Transaction Param),
    lsRules           :: Rules,
    lsRates           :: Rates,
    lsLoadedRecords   :: [Ext Record],
    lsTranQueue       :: Queue (Transaction Amount),
    lsMessages        :: TChan (Priority, String),
    lsConfig          :: LedgerOptions,
    -- | Source location of current transaction
    lsPosition        :: SourcePos
  }
  deriving (Eq)

instance Monad m => MonadState LedgerState (EMT l (LedgerStateT m)) where
  get = lift get
  put s = lift (put s)

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
             lsPosition = nowhere
           }

-- | Wrap IO action into EMT monad
wrapIO :: (MonadIO m, Throws InternalError l)
       => IO a
       -> EMT l m a
wrapIO action = wrapE $ liftIO action

stm :: Throws InternalError l => STM a -> LedgerSTM l a
stm action = EMT $ LedgerStateT $ StateT $ \ls -> do
               result <- action
               return (Right result, ls)

throwP e = do
  pos <- gets lsPosition
  throw (e pos)

-- | Set current source location
setPos :: (Monad m) => SourcePos -> LedgerT l m ()
setPos pos =
  modify $ \st -> st {lsPosition = pos}

logSTM :: Throws InternalError l => Priority -> String -> LedgerSTM l ()
logSTM priority message = do
  chan <- gets lsMessages
  stm $ writeTChan chan (priority, message)

infoSTM :: Throws InternalError l => String -> LedgerSTM l ()
infoSTM message = logSTM INFO message

warningSTM :: Throws InternalError l => String -> LedgerSTM l ()
warningSTM message = logSTM WARNING message

outputMessages :: TChan (Priority, String) -> IO ()
outputMessages chan = do
  x <- atomically $ tryReadTChan chan
  case x of
    Nothing -> return ()
    Just (priority, message) -> do
        logM rootLoggerName priority message
        outputMessages chan

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

readIOList :: (Throws InternalError l) => IOList a -> LedgerSTM l [a]
readIOList iolist = stm (readTVar iolist)

readIOListL :: Throws InternalError l => IOList a -> Ledger l [a]
readIOListL iolist = wrapIO $ atomically $ readTVar iolist

writeIOList :: (MonadIO m, Throws InternalError l) => IOList a -> [a] -> EMT l m ()
writeIOList iolist x = wrapIO (atomically $ writeTVar iolist x)

appendIOList :: (Throws InternalError l) => IOList a -> a -> LedgerSTM l ()
appendIOList iolist x =
  stm $ modifyTVar iolist (x:)

-- | Update last item of 'IOList'
plusIOList :: (Throws InternalError l)
           => a          -- ^ Default value (put it to list if it's empty)
           -> (a -> a)
           -> IOList a
           -> LedgerSTM l ()
plusIOList def fn iolist =
  stm $ modifyTVar iolist $ \list ->
    case list of
      [] -> [def]
      (x:xs) -> fn x: x: xs

modifyLastItem ::  (Throws InternalError l)
               => (a -> a)
               -> IOList (Ext a)
               -> LedgerSTM l ()
modifyLastItem fn iolist = do
  stm $ modifyTVar iolist $ \list ->
    case list of
      [] -> []
      (x:xs) -> (x {getContent = fn (getContent x)}): xs

