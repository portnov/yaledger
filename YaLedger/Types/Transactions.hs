
module YaLedger.Types.Transactions where

import Data.Dates

import YaLedger.Attributes
import YaLedger.Types.Common
import YaLedger.Types.Ledger
import YaLedger.Types.Rules

data Record =
    Template String (Transaction Param)
  | RuleR String Condition (Transaction Param)
  | Periodic String DateInterval (Transaction Amount)
  | StopPeriodic String
  | Transaction (Transaction Amount)
  deriving (Eq, Show)

data Transaction v =
    TEntry (Entry v Unchecked)
  | TReconciliate AnyAccount v
  | TCallTemplate String [Amount]
  | TSetRate Rate
  deriving (Eq, Show)

data Query = Query {
    qStart :: Maybe DateTime,
    qEnd   :: Maybe DateTime,
    qAllAdmin :: Bool,
    qAttributes :: Attributes }
  deriving (Eq, Show)

