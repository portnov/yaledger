{-# LANGUAGE GADTs, FlexibleContexts, PatternGuards #-}
module YaLedger.Processor.Duplicates
  (deduplicate) where

import Control.Applicative ((<$>))
import Control.Monad.Exception
import Control.Monad.Loc
import qualified Data.Map as M
import Data.Dates
import Data.Decimal

import YaLedger.Types
import YaLedger.Kernel.Correspondence (match)
import YaLedger.Output.Pretty
import YaLedger.Exceptions
import YaLedger.Logger
import YaLedger.Processor.DateIndex

-- | Find matching deduplication rule
findDRule :: Ext Record -> [DeduplicationRule] -> Maybe ([CheckAttribute], Attributes, DAction)
findDRule erecord rules = go  rules
  where
    go [] = Nothing
    go (r:rs)
      | checkDRule erecord r = Just (drCheckAttributes r, drOldRecordAttrs r, drAction r)
      | otherwise = go rs

-- | Check if record is matched by rule
checkDRule :: Ext Record -> DeduplicationRule -> Bool
checkDRule erecord rule =
  getAttributes erecord `match` drNewRecordAttrs rule

-- | Get first amount from record, if any
getRAmount :: Ext Record -> Maybe Amount
getRAmount r =
  case getContent r of
    Transaction (TEntry (UEntry dt cr _ _)) ->
      Just $ head $ map postingValue dt ++ map postingValue cr
    Transaction (TReconciliate _ _ x _ _) -> Just x
    _ -> Nothing

getCreditAccount :: Ext Record -> Maybe AccountID
getCreditAccount r =
  case getContent r of
    Transaction (TEntry (UEntry dr cr corr _)) ->
      case cr of
        [] -> if null dr then Nothing else (getID <$> corr)
        (CPosting acc _ _: _) -> Just (getID acc)
    _ -> Nothing

getDebitAccount :: Ext Record -> Maybe AccountID
getDebitAccount r =
  case getContent r of
    Transaction (TEntry (UEntry dr cr corr _)) ->
      case dr of
        [] -> if null cr then Nothing else (getID <$> corr)
        (DPosting acc _ _: _) -> Just (getID acc)
    _ -> Nothing

-- | Search for record with matching attributes in list.
-- Returns (Maybe found record, all records except found).
matchBy :: DateIndex (Ext Record)
        -> [CheckAttribute]                 -- ^ Attributes to match
        -> Attributes                       -- ^ Attributes of old records
        -> Ext Record                       -- ^ New record
        -> [Ext Record]                     -- ^ All other records
        -> (Maybe (Ext Record), [Ext Record])
matchBy index checks oldAttrs newRecord oldRecords
    | (CDate dx:_) <- checks = trace ("Matching " ++ show (extID newRecord)) $
                               case go (lookupDatePrev (getDate newRecord) dx index) of
                                 Nothing -> (Nothing, oldRecords)
                                 Just r -> (Just r,  filter (r /=) oldRecords)
    | otherwise = trace ("Matching " ++ show (extID newRecord)) $
                  case go oldRecords of
                    Nothing -> (Nothing, oldRecords)
                    Just r -> (Just r,  filter (r /=) oldRecords)
  where
    go [] = Nothing
    go (r:rs) = 
      if (newRecord /= r) && checkAttrs oldAttrs r && all (matches r) checks
        then Just r
        else go rs

    matches oldRecord (CDate d) =
      trace (show (extID newRecord) ++ " DATE: " ++ show (getDate newRecord) ++ ", " ++ show (getDate oldRecord)) $
      datesDifference (getDate newRecord) (getDate oldRecord) <= d
    matches oldRecord (CAmount x) =
      case (getRAmount newRecord, getRAmount oldRecord) of
        (Just (a1 :# c1), Just (a2 :# c2)) -> 
          if c1 /= c2
            then False
            else traceS (show (extID newRecord) ++ " A: " ++ show a1 ++ ", " ++ show a2) $
                 if x == 0
                   then a1 == a2
                   else (max a1 a2) *. (fromIntegral x / 100.0) > (abs $ a1 - a2)
        _ -> False
    matches oldRecord CCreditAccount =
      case (getCreditAccount newRecord, getCreditAccount oldRecord) of
        (Just aid1, Just aid2) -> trace (show (extID newRecord) ++ " CACC: " ++ show aid1 ++ ", " ++ show aid2) $ aid1 == aid2
        (Nothing,_) -> trace (show (extID newRecord) ++ " No credit account specified") False
        _                      -> False
    matches oldRecord CDebitAccount =
      case (getDebitAccount newRecord, getDebitAccount oldRecord) of
        (Just aid1, Just aid2) -> trace (show (extID newRecord) ++ " DACC: " ++ show aid1 ++ ", " ++ show aid2) $ aid1 == aid2
        (Nothing, Just _) -> trace (show (extID newRecord) ++ " No debit account specified") False
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

checkAttrs :: Attributes -> Ext Record -> Bool
checkAttrs qry rec = all (matches $ getAttributes rec) (M.assocs qry)
  where
    matches attrs (name, avalue) =
      case M.lookup name attrs of
        Nothing  -> False
        Just val -> matchAV val avalue

-- | Apply all deduplication rules
deduplicate :: (Throws DuplicatedRecord l,
                Throws InternalError l)
            => [DeduplicationRule]        -- ^ List of deduplication rules
            -> [Ext Record]               -- ^ Source list of records
            -> Ledger l [Ext Record]
deduplicate rules records = go $ reverse records
  where
    index = buildIndex records

    go [] = return []
    go (r:rs) =
      case findDRule r rules of
        Nothing -> trace ("No rule for " ++ show (extID r)) $ (r:) <$> go rs
        Just (checks, oldAttrs, action) -> 
          case matchBy index checks oldAttrs r rs of
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

                 
