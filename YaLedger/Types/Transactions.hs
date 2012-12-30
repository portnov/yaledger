{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

module YaLedger.Types.Transactions where

import qualified Data.Map as M
import Data.Dates

import YaLedger.Types.Attributes
import YaLedger.Types.Common
import YaLedger.Types.Ledger

data Record =
    Template String (Transaction Param)
  | RuleR String Condition (Transaction Param)
  | Periodic String DateInterval (Transaction Amount)
  | StopPeriodic String
  | SetRate [Rate]
  | Transaction (Transaction Amount)
  deriving (Eq, Show)

data Transaction v =
    TEntry (Entry v Unchecked)
  | TReconciliate AnyAccount v (Maybe AnyAccount) (Maybe ReconciliationMessage)
  | TCallTemplate String [Amount]
  | THold [Hold v Credit] [Hold v Debit]
  | TCloseHolds [CloseHold v Credit] [CloseHold v Debit]
  deriving (Eq, Show)

data CloseHold v t = CloseHold {
    holdToClose :: Hold v t,
    searchLesserAmount :: Bool,
    searchAttributes :: Attributes }
  deriving (Show)

deriving instance (Eq (Posting v t)) => Eq (CloseHold v t)

data ReconciliationMessage =
    RWarning MessageFormat
  | RError MessageFormat
  deriving (Eq, Show)

data Query = Query {
    qStart :: Maybe DateTime,
    qEnd   :: Maybe DateTime,
    qAllAdmin :: Bool,
    qAttributes :: Attributes }
  deriving (Eq, Show)

data Condition =
  Condition {
    cAccounts :: [AccountID],
    cGroups :: [GroupID],
    cAction :: Maybe PostingType,
    cAttributes :: Attributes,
    cValue  :: ValueCondition }
  deriving (Eq, Show)

data ValueCondition =
    AnyValue
  | MoreThan Amount
  | LessThan Amount
  | Equals Amount
  deriving (Eq, Show)

data Rule = When Condition (Transaction Param)
  deriving (Eq, Show)

data DateZipper a =
    EmptyZipper
  | DateZipper {
      dzDate :: DateTime,
      dzThisDate :: [a],
      dzPrevDate :: Maybe (DateZipper a),
      dzNextDate :: Maybe (DateZipper a) }
  deriving (Eq, Show)

type DateIndex a = M.Map Int (M.Map Int (M.Map Int (DateZipper a)))

replicateRecords :: DateInterval -> Int -> [Ext Record] -> [Ext Record]
replicateRecords interval n list = concat [map (shift i) list | i <- [0..n]]
  where
    shift 0 r = r
    shift i r = r {getDate = plus i (getDate r)}

    plus 0 date = date
    plus i date = plus (i-1) date `addInterval` interval

