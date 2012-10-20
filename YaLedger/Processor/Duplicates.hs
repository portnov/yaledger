{-# LANGUAGE GADTs, FlexibleContexts, PatternGuards #-}
module YaLedger.Processor.Duplicates
  (SetValue (..), SetAttribute (..),
   CheckAttribute (..), DAction (..),
   DeduplicationRule (..),
   deduplicate
  ) where

import Control.Applicative ((<$>))
import Control.Monad.Exception
import Control.Monad.Loc
import qualified Data.Map as M
import Data.Dates
import Data.Decimal

import YaLedger.Types
import YaLedger.Kernel.Correspondence (matchAll)
import YaLedger.Output.Pretty
import YaLedger.Exceptions
import YaLedger.Monad
import YaLedger.Logger

-- | Which attribute to set
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
    drCondition :: Attributes              -- ^ Rule is applicable only to records with this attributes
  , drCheckAttributes :: [CheckAttribute]  -- ^ What attributes to check for duplication
  , drAction :: DAction                    -- ^ What to do with duplicated record
  }
  deriving (Eq)

instance Show DeduplicationRule where
  show dr = showA (drCondition dr) ++ " => " ++ show (drAction dr)

-- | Find matching deduplication rule
findDRule :: Ext Record -> [DeduplicationRule] -> Maybe ([CheckAttribute], DAction)
findDRule erecord rules = go  rules
  where
    go [] = Nothing
    go (r:rs)
      | checkDRule erecord r = Just (drCheckAttributes r, drAction r)
      | otherwise = go rs

-- | Check if record is matched by rule
checkDRule :: Ext Record -> DeduplicationRule -> Bool
checkDRule erecord rule =
  getAttributes erecord `matchAll` drCondition rule

-- | Get first amount from record, if any
getRAmount :: Ext Record -> Maybe Amount
getRAmount r =
  case getContent r of
    Transaction (TEntry (UEntry dt cr _ _)) ->
      Just $ head $ map postingValue dt ++ map postingValue cr
    Transaction (TReconciliate _ x) -> Just x
    _ -> Nothing

getCreditAccount :: Ext Record -> Maybe AccountID
getCreditAccount r =
  case getContent r of
    Transaction (TEntry (UEntry _ cr _ _)) ->
      case cr of
        [] -> Nothing
        (CPosting acc _: _) -> Just (getID acc)
    _ -> Nothing

getDebitAccount :: Ext Record -> Maybe AccountID
getDebitAccount r =
  case getContent r of
    Transaction (TEntry (UEntry dt _ _ _)) ->
      case dt of
        [] -> Nothing
        (DPosting acc _: _) -> Just (getID acc)
    _ -> Nothing

-- | Search for record with matching attributes in list.
-- Returns (Maybe found record, all records except found).
matchBy :: [CheckAttribute]                 -- ^ Attributes to match
        -> Ext Record                       -- ^ New record
        -> [Ext Record]                     -- ^ All other records
        -> (Maybe (Ext Record), [Ext Record])
matchBy checks newRecord oldRecords = go oldRecords
  where
    go [] = (Nothing, [])
    go (r:rs) = 
      if all (matches r) checks
        then (Just r, rs)
        else let (x, ers) = go rs
             in  (x, r: ers)

    matches oldRecord (CDate d) =
      datesDifference (getDate newRecord) (getDate oldRecord) <= d
    matches oldRecord (CAmount x) =
      case (getRAmount newRecord, getRAmount oldRecord) of
        (Just (a1 :# c1), Just (a2 :# c2)) -> 
          if c1 /= c2
            then False
            else traceS ("A: " ++ show a1 ++ ", " ++ show a2) $
                 if x == 0
                   then a1 == a2
                   else (max a1 a2) *. (fromIntegral x / 100.0) > (abs $ a1 - a2)
        _ -> False
    matches oldRecord CCreditAccount =
      case (getCreditAccount newRecord, getCreditAccount oldRecord) of
        (Just aid1, Just aid2) -> trace ("CACC: " ++ show aid1 ++ ", " ++ show aid2) $ aid1 == aid2
        _                      -> False
    matches oldRecord CDebitAccount =
      case (getDebitAccount newRecord, getDebitAccount oldRecord) of
        (Just aid1, Just aid2) -> trace ("DACC: " ++ show aid1 ++ ", " ++ show aid2) $ aid1 == aid2
        (Nothing, Nothing)     -> True
        _                      -> False
    matches oldRecord (CAttribute name) =
      case (getAttr name newRecord, getAttr name oldRecord) of
        (Just v1, Just v2) -> v1 == v2
        (Nothing, Nothing)     -> True
        _                  -> False

-- | Set attributes from new record to old record
setAttributes :: [SetAttribute]  -- ^ Which attributes to set
              -> Ext Record      -- ^ New record
              -> Ext Record      -- ^ Older record
              -> Ext Record
setAttributes sets newRecord oldRecord = foldl apply oldRecord sets
  where
    apply :: Ext Record -> SetAttribute -> Ext Record
    apply record (targetName := src)
      | targetName == "date" = record {getDate = getDate newRecord}
      | SExactly "date" <- src = 
          setAttr targetName (Exactly $ prettyPrint $ getDate newRecord) record
      | SOptional "date" <- src = 
          setAttr targetName (Optional $ prettyPrint $ getDate newRecord) record
      | SExactly sourceName <- src = 
          case getAttr sourceName newRecord of
            Nothing -> record
            Just value -> setAttr targetName (Exactly value) record
      | SOptional sourceName <- src = 
          case getAttr sourceName newRecord of
            Nothing -> record
            Just value -> setAttr targetName (Optional value) record
      | SFixed string <- src = 
          setAttr targetName (Exactly string) record
      | otherwise = record

getAttr :: String -> Ext Record -> Maybe String
getAttr key rec = getString <$> M.lookup key (getAttributes rec)

setAttr :: String -> AttributeValue -> Ext Record -> Ext Record
setAttr name value rec =
    rec {getAttributes = M.insert name value (getAttributes rec)}

-- | Apply all deduplication rules
deduplicate :: (Throws DuplicatedRecord l,
                Throws InternalError l)
            => [DeduplicationRule]        -- ^ List of deduplication rules
            -> [Ext Record]               -- ^ Source list of records
            -> Ledger l [Ext Record]
deduplicate rules records = go (reverse records)
  where
    go [] = return []
    go (r:rs) =
      case findDRule r rules of
        Nothing -> (r:) <$> go rs
        Just (checks, action) -> 
          case matchBy checks r rs of
            (Nothing, _) -> (r:) <$> go rs
            (Just old, other) -> 
              case action of
                DError -> throw $ DuplicatedRecord
                                      (prettyPrint r ++ "\nOld was:\n" ++ prettyPrint old)
                                      (getLocation r)
                DWarning -> do
                    warning $ "Duplicated records:\n" ++ prettyPrint r ++
                              "\nOld was:\n" ++ prettyPrint old
                    (r:) <$> go rs
                DDuplicate -> (r:) <$> go rs
                DIgnoreNew -> go rs
                DDeleteOld -> (r:) <$> go other
                DSetAttributes sets ->
                    (setAttributes sets r old:) <$> go other

                 
