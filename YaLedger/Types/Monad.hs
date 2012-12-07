{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}
-- | 'Ledger' monad and utility functions.
--
-- Ledger monad is basically EMT l (StateT 'LedgerState' IO) a.
--
module YaLedger.Types.Monad where

import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Control.Concurrent.STM
import Data.Dates
import qualified Data.Map as M
import Text.Parsec.Pos

import YaLedger.Types.Attributes
import YaLedger.Types.Common
import YaLedger.Types.Ledger
import YaLedger.Types.Map
import YaLedger.Types.Transactions
import YaLedger.Types.Config
import YaLedger.Tree
import YaLedger.Exceptions

newtype LedgerMonad a = LedgerMonad (StateT LedgerState IO a)
  deriving (Monad, MonadState LedgerState, MonadIO)

type Ledger l a = EMT l LedgerMonad a

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
    lsConfig          :: LedgerOptions,
    -- | Source location of current transaction
    lsPosition        :: SourcePos
  }
  deriving (Eq, Show)

instance MonadState LedgerState (EMT l LedgerMonad) where
  get = lift get
  put s = lift (put s)

emptyLedgerState :: LedgerOptions -> ChartOfAccounts -> AccountMap -> [Ext Record] -> IO LedgerState
emptyLedgerState opts coa amap records = do
  now <- getCurrentDateTime
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
             lsConfig = opts,
             lsPosition = newPos "<nowhere>" 0 0
           }

-- | Wrap IO action into EMT monad
wrapIO :: (MonadIO m, Throws InternalError l)
       => IO a
       -> EMT l m a
wrapIO action = wrapE $ liftIO action

throwP e = do
  pos <- gets lsPosition
  throw (e pos)

-- | Set current source location
setPos :: SourcePos -> Ledger l ()
setPos pos =
  modify $ \st -> st {lsPosition = pos}

runLedger :: LedgerOptions -> ChartOfAccounts -> AccountMap -> [Ext Record] -> LedgerMonad a -> IO a
runLedger opts coa amap records action = do
  let LedgerMonad emt = action
  st <- emptyLedgerState opts coa amap records
  -- Use currency of root accounts group as default currency
  (res, _) <- runStateT emt (st {lsDefaultCurrency = agCurrency $ branchData coa})
  return res

-- * IOList
--
newIOList :: (MonadIO m, Throws InternalError l) => EMT l m (IOList a)
newIOList = wrapIO $ newTVarIO []

readIOList :: (MonadIO m, Throws InternalError l) => IOList a -> EMT l m [a]
readIOList iolist = wrapIO (readTVarIO iolist)

writeIOList :: (MonadIO m, Throws InternalError l) => IOList a -> [a] -> EMT l m ()
writeIOList iolist x = wrapIO (atomically $ writeTVar iolist x)

appendIOList :: (MonadIO m, Throws InternalError l) => IOList a -> a -> EMT l m ()
appendIOList iolist x =
  wrapIO $ atomically $ modifyTVar iolist (x:)

-- | Update last item of 'IOList'
plusIOList :: (MonadIO m, Throws InternalError l)
           => a          -- ^ Default value (put it to list if it's empty)
           -> (a -> a)
           -> IOList a
           -> EMT l m ()
plusIOList def fn iolist = do
  wrapIO $ atomically $ modifyTVar iolist $ \list ->
    case list of
      [] -> [def]
      (x:xs) -> fn x: x: xs

modifyLastItem ::  (MonadIO m, Throws InternalError l)
               => (a -> a)
               -> IOList (Ext a)
               -> EMT l m ()
modifyLastItem fn iolist = do
  wrapIO $ atomically $ modifyTVar iolist $ \list ->
    case list of
      [] -> []
      (x:xs) -> (x {getContent = fn (getContent x)}): xs

