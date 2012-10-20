{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}
-- | 'Ledger' monad and utility functions.
--
-- Ledger monad is basically EMT l (StateT 'LedgerState' IO) a.
--
module YaLedger.Monad where

import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Data.Dates
import qualified Data.Map as M
import Data.IORef
import Text.Parsec.Pos

import YaLedger.Types
import YaLedger.Types.Common
import YaLedger.Exceptions
import YaLedger.Types.Config

newtype LedgerMonad a = LedgerMonad (StateT LedgerState IO a)
  deriving (Monad, MonadState LedgerState, MonadIO)

type Ledger l a = EMT l LedgerMonad a

-- | Ledger state
data LedgerState = LedgerState {
    lsStartDate       :: DateTime,
    lsDefaultCurrency :: Currency,
    lsCoA             :: ChartOfAccounts,
    lsAccountMap      :: AccountMap,
    lsTemplates       :: M.Map String (Attributes, Transaction Param),
    lsRules           :: [(String, Attributes, Rule)],
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
             lsTemplates = M.empty,
             lsRules = [],
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
newIOList = wrapIO $ newIORef []

readIOList :: (MonadIO m, Throws InternalError l) => IOList a -> EMT l m [a]
readIOList iolist = wrapIO (readIORef iolist)

appendIOList :: (MonadIO m, Throws InternalError l) => IOList a -> a -> EMT l m ()
appendIOList iolist x = wrapIO $ modifyIORef iolist (x:)

-- | Update last item of 'IOList'
plusIOList :: (MonadIO m, Throws InternalError l)
           => a          -- ^ Default value (put it to list if it's empty)
           -> (a -> a)
           -> IOList a
           -> EMT l m ()
plusIOList def fn iolist = do
  wrapIO $ modifyIORef iolist $ \list ->
    case list of
      [] -> [def]
      (x:xs) -> fn x: x: xs

modifyLastItem ::  (MonadIO m, Throws InternalError l)
               => (a -> a)
               -> IOList (Ext a)
               -> EMT l m ()
modifyLastItem fn iolist = do
  wrapIO $ modifyIORef iolist $ \list ->
    case list of
      [] -> []
      (x:xs) -> (x {getContent = fn (getContent x)}): xs

