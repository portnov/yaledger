{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module YaLedger.Types.Monad where

import Control.Monad.Reader.Class
import Control.Monad.State
import Control.Monad.Exception
import Control.Concurrent.STM
import Data.Dates
import qualified Data.Map as M
import Text.Parsec.Pos
import System.Log.Logger (Priority (..))

import YaLedger.Types.Attributes
import YaLedger.Types.Common
import YaLedger.Types.Ledger
import YaLedger.Types.Map
import YaLedger.Types.Transactions
import YaLedger.Types.Config

-- | Base layer of Ledger monad: StateT transformer.
newtype LedgerStateT m a = LedgerStateT (StateT LedgerState m a)
  deriving (Monad, MonadState LedgerState, MonadIO)

-- | Generic type of YaLedger monad. This is used in
-- two instances: 'Ledger' and 'Atomic'.
type LedgerT l m a = EMT l (LedgerStateT m) a

-- | This monad is used in most computations,
-- which do not need to lock any accounts.
type Ledger l a = LedgerT l IO a

-- | Computations of this type are peformed
-- atomically, inside STM transaction.
type Atomic l a = LedgerT l STM a

data Rules = Rules {
    creditRules :: [(String, Attributes, Rule)]
  , debitRules  ::  [(String, Attributes, Rule)]
  }
  deriving (Eq, Show)

-- | Ledger state
data LedgerState = LedgerState {
    lsStartDate       :: DateTime                                      -- ^ Date/Time of YaLedger startup.
  , lsDefaultCurrency :: Currency
  , lsCoA             :: ChartOfAccounts
  , lsAccountMap      :: AccountMap
  , lsFullGroupsMap   :: M.Map AccountID [GroupID]                     -- ^ For each account ID, stores list of IDs of all groups account belongs to.
  , lsTemplates       :: M.Map String (Attributes, Transaction Param)  -- ^ Templates
  , lsRules           :: Rules
  , lsRates           :: Rates                                         -- ^ Exchange rates.
  , lsLoadedRecords   :: [Ext Record]                                  -- ^ All records loaded from source files.
  , lsTranQueue       :: Queue (Transaction Amount)                    -- ^ Transactions queue.
  , lsMessages        :: TChan (String, Priority, String)              -- ^ Log messages queue. Is used to output messages from 'Atomic' transactions
  , lsConfig          :: LedgerOptions                                 -- ^ Configuration options
  , lsPosition        :: SourcePos                                     -- ^ Source location of current transaction
  }
  deriving (Eq)

instance Monad m => MonadState LedgerState (EMT l (LedgerStateT m)) where
  get = lift get
  put s = lift (put s)

instance Monad m => MonadReader LedgerOptions (EMT l (LedgerStateT m)) where
  ask = gets lsConfig

  local fn m = do
    st <- get
    put $ st {lsConfig = fn (lsConfig st)}
    x <- m
    put st
    return x

