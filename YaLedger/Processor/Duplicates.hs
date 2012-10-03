{-# LANGUAGE GADTs, FlexibleContexts #-}
{- OPTIONS_GHC -F -pgmF MonadLoc #-}
module YaLedger.Processor.Duplicates where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Exception
import Control.Monad.Loc
import qualified Data.Map as M
import Data.Dates
import Data.Decimal
import System.Log.Logger

import YaLedger.Types
import YaLedger.Correspondence (matchAll)
import YaLedger.Pretty
import YaLedger.Exceptions
import YaLedger.Monad
import YaLedger.Logger

data SetAttribute =
    String := SetValue
  deriving (Eq, Show)

data SetValue =
    SExactly String
  | SOptional String
  | SFixed String
  deriving (Eq, Show)

data CheckAttribute =
    CDate Integer
  | CAmount Int
  | CCreditAccount
  | CDebitAccount
  | CAttribute String
  deriving (Eq, Show)

data DAction =
    DError
  | DWarning
  | DDuplicate
  | DIgnoreNew
  | DDeleteOld
  | DSetAttributes [SetAttribute]
  deriving (Eq, Show)

data DeduplicationRule =
  DeduplicationRule {
    drCondition :: Attributes,
    drCheckAttributes :: [CheckAttribute],
    drAction :: DAction }
  deriving (Eq)

instance Show DeduplicationRule where
  show dr = showA (drCondition dr) ++ " => " ++ show (drAction dr)

findDRule :: Ext Record -> [DeduplicationRule] -> Maybe ([CheckAttribute], DAction)
findDRule erecord rules = go  rules
  where
    go [] = Nothing
    go (r:rs)
      | checkDRule erecord r = Just (drCheckAttributes r, drAction r)
      | otherwise = go rs

checkDRule :: Ext Record -> DeduplicationRule -> Bool
checkDRule erecord rule =
  getAttributes erecord `matchAll` drCondition rule

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

matchBy :: [CheckAttribute] -> Ext Record -> [Ext Record] -> (Maybe (Ext Record), [Ext Record])
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
            else (max a1 a2) *. (fromIntegral x / 100.0) > (abs $ a1 - a2)
        _ -> False
    matches oldRecord CCreditAccount =
      case (getCreditAccount newRecord, getCreditAccount oldRecord) of
        (Just aid1, Just aid2) -> aid1 == aid2
        _                      -> False
    matches oldRecord CDebitAccount =
      case (getDebitAccount newRecord, getDebitAccount oldRecord) of
        (Just aid1, Just aid2) -> aid1 == aid2
        (Nothing, Nothing)     -> True
        _                      -> False
    matches oldRecord (CAttribute name) =
      case (getAttr name newRecord, getAttr name oldRecord) of
        (Just v1, Just v2) -> v1 == v2
        (Nothing, Nothing)     -> True
        _                  -> False

setAttributes :: [SetAttribute] -> Ext Record -> Ext Record -> Ext Record
setAttributes sets newRecord oldRecord = foldl apply oldRecord sets
  where
    apply :: Ext Record -> SetAttribute -> Ext Record
    apply rec (targetName := src)
      | targetName == "date" = rec {getDate = getDate newRecord}
      | SExactly "date" <- src = 
          setAttr targetName (Exactly $ prettyPrint $ getDate newRecord) rec
      | SOptional "date" <- src = 
          setAttr targetName (Optional $ prettyPrint $ getDate newRecord) rec
      | SExactly sourceName <- src = 
          case getAttr sourceName newRecord of
            Nothing -> rec
            Just value -> setAttr targetName (Exactly value) rec
      | SOptional sourceName <- src = 
          case getAttr sourceName newRecord of
            Nothing -> rec
            Just value -> setAttr targetName (Optional value) rec
      | SFixed string <- src = 
          setAttr targetName (Exactly string) rec
      | otherwise = rec

getAttr :: String -> Ext Record -> Maybe String
getAttr key rec = getString <$> M.lookup key (getAttributes rec)

setAttr :: String -> AttributeValue -> Ext Record -> Ext Record
setAttr name value rec =
    rec {getAttributes = M.insert name value (getAttributes rec)}

deduplicate :: (Throws DuplicatedRecord l,
                Throws InternalError l)
            => [DeduplicationRule]
            -> [Ext Record]
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

                 
