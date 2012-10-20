
module YaLedger.Types.Transactions where

import Data.Dates

import YaLedger.Types.Attributes
import YaLedger.Types.Common
import YaLedger.Types.Ledger
import YaLedger.Types.Rules

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
  | TReconciliate AnyAccount v
  | TCallTemplate String [Amount]
  deriving (Eq, Show)

data Query = Query {
    qStart :: Maybe DateTime,
    qEnd   :: Maybe DateTime,
    qAllAdmin :: Bool,
    qAttributes :: Attributes }
  deriving (Eq, Show)

replicateRecords :: DateInterval -> Int -> [Ext Record] -> [Ext Record]
replicateRecords interval n list = concat [map (shift i) list | i <- [0..n]]
  where
    shift 0 r = r
    shift i r = r {getDate = plus i (getDate r)}

    plus 0 date = date
    plus i date = plus (i-1) date `addInterval` interval

