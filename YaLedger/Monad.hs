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
import Text.Parsec.Pos

import YaLedger.Types
import YaLedger.Exceptions

newtype LedgerMonad a = LedgerMonad (StateT LedgerState IO a)
  deriving (Monad, MonadState LedgerState, MonadIO)

type Ledger l a = EMT l LedgerMonad a

data LedgerState = LedgerState {
    lsStartDate :: DateTime,
    lsDefaultCurrency :: Currency,
    lsCoA :: ChartOfAccounts,
    lsAccountMap :: AccountMap,
    lsTemplates :: M.Map String (Attributes, Transaction Param),
    lsRules :: [(String, Attributes, Rule)],
    lsRates :: Rates,
    lsLoadedRecords :: [Ext Record],
    lsPosition :: SourcePos
  }
  deriving (Eq, Show)

instance MonadState LedgerState (EMT l LedgerMonad) where
  get = lift get
  put s = lift (put s)

emptyLedgerState :: ChartOfAccounts -> AccountMap -> [Ext Record] -> IO LedgerState
emptyLedgerState coa amap records = do
  now <- getCurrentDateTime
  return $ LedgerState {
             lsStartDate = now,
             lsDefaultCurrency = "",
             lsCoA = coa,
             lsAccountMap = amap,
             lsTemplates = M.empty,
             lsRules = [],
             lsRates = M.empty,
             lsLoadedRecords = records,
             lsPosition = newPos "<nowhere>" 0 0
           }

wrapIO :: (MonadIO m, Throws InternalError l)
       => IO a
       -> EMT l m a
wrapIO action = wrapE $ liftIO action

type EHandler e = [String] -> e -> Ledger NoExceptions ()

handler loc e =
  wrapIO (putStrLn $ showExceptionWithTrace loc e)

throwP e = do
  pos <- gets lsPosition
  throw (e pos)

setPos :: SourcePos -> Ledger l ()
setPos pos =
  modify $ \st -> st {lsPosition = pos}

message :: Throws InternalError l => String -> Ledger l ()
message str =
  wrapIO $ putStrLn $ ">> " ++ str

runLedger :: ChartOfAccounts -> AccountMap -> [Ext Record] -> LedgerMonad a -> IO a
runLedger coa amap records action = do
  let LedgerMonad emt = action
  st <- emptyLedgerState coa amap records
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

plusIOList :: (MonadIO m, Throws InternalError l)
           => a
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

