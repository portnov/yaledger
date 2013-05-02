
module YaLedger.Types.Config
  (LedgerOptions (..),
   SetOption (..),
   SetValue (..), SetAttribute (..),
   CheckAttribute (..), DAction (..),
   DeduplicationRule (..)
  ) where

import Data.Dates
import System.Log.Logger
import qualified Data.Map as M

import YaLedger.Types.Attributes
import YaLedger.Types.Transactions

data LedgerOptions =
    LedgerOptions {
      mainConfigPath :: Maybe FilePath,
      chartOfAccounts :: Maybe FilePath,
      accountMap :: Maybe FilePath,
      currenciesList :: Maybe FilePath,
      files :: [FilePath],
      query :: Query,
      reportsInterval :: Maybe DateInterval,
      reportsQuery :: Query,
      assetAccounts :: Attributes,
      liabilityAccounts :: Attributes,
      incomeAccounts :: Attributes,
      expenceAccounts :: Attributes,
      defaultLogSeverity :: Priority,
      logSeveritySetup :: [(String, Priority)],
      parserConfigs :: [(String, FilePath)],
      deduplicationRules :: [DeduplicationRule],
      defaultReport :: String,
      defaultReportParams :: M.Map String String,
      reportParams :: [String],
      outputFile :: Maybe FilePath,
      colorizeOutput :: Bool }
  | Help
  deriving (Eq, Show)

data SetOption =
    SetConfigPath FilePath
  | SetCoAPath FilePath
  | SetAMapPath FilePath
  | SetCurrenciesPath FilePath
  | AddFile (Maybe FilePath)
  | SetStartDate DateTime
  | SetEndDate DateTime
  | SetAllAdmin
  | AddAttribute (String, AttributeValue)
  | SetReportStart DateTime
  | SetReportEnd DateTime
  | SetReportsInterval DateInterval
  | SetDebugLevel (String, Priority)
  | SetParserConfig (String, FilePath)
  | SetOutputFile FilePath
  | SetColorizeOutput
  | SetHelp
  deriving (Eq,Show)

-- | Which attribute to set by deduplication rule
data SetAttribute =
    String := SetValue
  deriving (Eq, Show)

-- | Type of set attribute value
data SetValue =
    SExactly String    -- ^ Get value from named attribute, set it 'Exactly'
  | SOptional String   -- ^ Get value from named attribute, set it 'Optional'
  | SFixed String      -- ^ Set value to 'Exactly' (given string)
  deriving (Eq, Show)

-- | Attributes to check for duplication
data CheckAttribute =
    CDate Integer      -- ^ Date/time. Argument is allowed divergence in days.
  | CAmount Int        -- ^ Record amount. Argument allowed divergence in percents.
  | CCreditAccount     -- ^ Credit accounts should match
  | CDebitAccount      -- ^ Debit accounts should match
  | CAttribute String  -- ^ This named attribute should match
  deriving (Eq, Show)

-- | What to do with duplicated record
data DAction =
    DError                          -- ^ Raise an error and exit
  | DWarning                        -- ^ Produce a warning
  | DDuplicate                      -- ^ Leave duplicated record, do nothing
  | DIgnoreNew                      -- ^ Ignore new record
  | DDeleteOld                      -- ^ Delete old record
  | DSetAttributes [SetAttribute]   -- ^ Ignore new record, but set this attributes from new record to old one
  deriving (Eq, Show)

-- | Rule to deduplicate records
data DeduplicationRule =
  DeduplicationRule {
    drNewRecordAttrs :: Attributes         -- ^ Rule is applicable only to records with this attributes
  , drOldRecordAttrs :: Attributes         -- ^ Old records with this attributes are searched 
  , drCheckAttributes :: [CheckAttribute]  -- ^ What attributes to check for duplication
  , drAction :: DAction                    -- ^ What to do with duplicated record
  }
  deriving (Eq)

instance Show DeduplicationRule where
  show dr = showA (drNewRecordAttrs dr) ++ " => " ++ show (drAction dr)

