{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

module YaLedger.Types.Transactions where

import qualified Data.Map as M
import Data.Dates

import YaLedger.Types.Attributes
import YaLedger.Types.Common
import YaLedger.Types.Ledger

-- | Input records.
data Record =
    Template String (Transaction Param)
  | RuleR String Condition (Transaction Param)
  | Periodic String DateInterval (Transaction Amount)
  | StopPeriodic String
  | SetRate [Rate]
  | Transaction (Transaction Amount)
  deriving (Eq, Show)

-- | Transaction. `v' usually is Param or Amount.
data Transaction v =
    TEntry (Entry v Unchecked)
  | TReconciliate BalanceType AnyAccount v (Maybe AnyAccount) (Maybe ReconciliationMessage)
  | TCallTemplate String [Amount]
  | THold [Hold v Credit] [Hold v Debit]
  | TCloseHolds [CloseHold v Credit] [CloseHold v Debit]
  deriving (Eq, Show)

-- | Query to close the hold
data CloseHold v t = CloseHold {
    holdToClose :: Hold v t        -- ^ Hold to close
  , searchLesserAmount :: Bool     -- ^ If True, then search for hold with amount <= specified, otherwise — ==.
  , searchAttributes :: Attributes -- ^ Attributes of hold to be found
  }
  deriving (Show)

deriving instance (Eq (Posting v t)) => Eq (CloseHold v t)

-- | Message to be output when account's balance is
-- not equal to specified value
data ReconciliationMessage =
    RWarning MessageFormat
  | RError MessageFormat
  deriving (Eq, Show)

-- | Query to select records, holds, etc.
data Query = Query {
    qStart :: Maybe DateTime   -- ^ If Just date, then search for records with recordDate => date
  , qEnd   :: Maybe DateTime   -- ^ If Just date, then search for records with recordDate <= date
  , qAllAdmin :: Bool          -- ^ If True, then include all non-financial records, with any dates and attributes
  , qAttributes :: Attributes  -- ^ Attributes of records to be found
  }
  deriving (Eq, Show)

-- | Conditions for rules (automatically-created transactions).
data Condition =
  Condition {
    cAccounts :: [AccountID] 
  , cGroups :: [GroupID]
  , cAction :: Maybe PostingType -- ^ If Nothing, then fire the rule on any usage of specified accounts\/groups
  , cAttributes :: Attributes    -- ^ Fire the rule only when source transaction has this attributes
  , cValue  :: ValueCondition }
  deriving (Eq, Show)

data ValueCondition =
    AnyValue
  | MoreThan Amount
  | LessThan Amount
  | Equals Amount
  deriving (Eq, Show)

-- | A rule: when condition is satisfied, create a transaction.
data Rule = When Condition (Transaction Param)
  deriving (Eq, Show)

-- | A Zipper into list of records.
data DateZipper a =
    EmptyZipper
  | DateZipper {
      dzDate :: DateTime,
      dzThisDate :: [a],
      dzPrevDate :: Maybe (DateZipper a),
      dzNextDate :: Maybe (DateZipper a) }
  deriving (Eq, Show)

-- | Index for records list: Map Year (Map Month (Map Day (DateZipper)))
type DateIndex a = M.Map Int (M.Map Int (M.Map Int (DateZipper a)))

-- | For stress-testing
replicateRecords :: DateInterval -> Int -> [Ext Record] -> [Ext Record]
replicateRecords interval n list = concat [map (shift i) list | i <- [0..n]]
  where
    shift 0 r = r
    shift i r = r {getDate = plus i (getDate r)}

    plus 0 date = date
    plus i date = plus (i-1) date `addInterval` interval

