
module YaLedger.Types.Rules where

import YaLedger.Attributes
import YaLedger.Types.Common
import YaLedger.Types.Ledger

data Condition =
  Condition {
    cAccounts :: [AccountID],
    cGroups :: [GroupID],
    cAction :: Maybe PostingType,
    cValue  :: ValueCondition }
  deriving (Eq, Show)

data ValueCondition =
    AnyValue
  | MoreThan Amount
  | LessThan Amount
  | Equals Amount
  deriving (Eq, Show)

