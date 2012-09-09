{-# LANGUAGE EmptyDataDecls, GADTs, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeSynonymInstances, DeriveDataTypeable, RecordWildCards, ScopedTypeVariables #-}

module Types where

import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc
import Control.Monad.Trans
import Data.List
import Data.Dates
import Data.Decimal
import qualified Data.Map as M
import Text.Regex.PCRE

import Tree

data Checked
data Unchecked

data Credit
data Debit
data Free

type Attributes = [(String, String)]

showA :: Attributes -> String
showA attrs = "{" ++ intercalate ", " (map one attrs) ++ "}"
  where
    one (name, value) = name ++ " = \"" ++ value ++ "\""

data Ext a =
  Ext {
    getDate :: DateTime,
    getAttributes :: Attributes,
    getContent :: a }
  deriving (Eq, Show)

instance Eq a => Ord (Ext a) where
  compare x y = compare (getDate x) (getDate y)

data TransactionData =
    TPosting (Posting Unchecked)
  | TReconcilate Path Amount
  | TInitlalize Path Amount

data Posting c where
    CPosting :: {
      cPostingDebitEntries :: [Entry Debit],
      cPostingCreditEntries :: [Entry Credit]
    } -> Posting Checked

    UPosting :: {
      uPostingDebitEntries :: [Entry Debit],
      uPostingCreditEntries :: [Entry Credit]
    } -> Posting Unchecked

data Amount = Decimal :# Currency
  deriving (Eq)

instance Show Amount where
  show (n :# c) = show n ++ c

type Currency = String

type Rates = M.Map (Currency, Currency) Double

type FreeOr t f = Either (f Free) (f t)

data Entry t where
  DEntry :: {
    debitEntryAccount :: FreeOr Debit Account,
    debitEntryAmount :: Amount
  } -> Entry Debit

  CEntry :: {
    creditEntryAccount :: FreeOr Credit Account,
    creditEntryAmount  :: Amount
  } -> Entry Credit

instance Show (Entry t) where
  show (DEntry acc x) = "debit " ++ getName acc ++ " by " ++ show x
  show (CEntry acc x) = "credit " ++ getName acc ++ " by " ++ show x

instance HasCurrency (Entry Debit) where
  getCurrency (DEntry _ (_ :# c)) = c

instance HasCurrency (Entry Credit) where
  getCurrency (CEntry _ (_ :# c)) = c

data EntryType =
    EDebit
  | ECredit
  deriving (Eq)

instance Show EntryType where
  show EDebit  = "debit"
  show ECredit = "credit"

class HasAmount a where
  getAmount :: a -> Amount

instance HasAmount (Entry t) where
  getAmount (DEntry _ x) = x
  getAmount (CEntry _ x) = x

instance HasAmount a => HasAmount (Ext a) where
  getAmount x = getAmount (getContent x)

class HasCurrency a where
  getCurrency :: a -> Currency

data Account t where
  CAccount :: {
    creditAccountName :: String,
    creditAccountCurrency :: Currency,
    creditAccountEntries :: [Ext (Entry Credit)]
  } -> Account Credit

  DAccount :: {
    debitAccountName :: String,
    debitAccountCurrency :: Currency,
    debitAccountEntries :: [Ext (Entry Debit)]
  } -> Account Debit

  FAccount :: {
    freeAccountName :: String,
    freeAccountCurrency :: Currency,
    freeAccountCreditEntries :: [Ext (Entry Credit)],
    freeAccountDebitEntries :: [Ext (Entry Debit)]
  } -> Account Free

instance Eq (Account t) where
  a1 == a2 = (getName a1 == getName a2) &&
             (getCurrency a1 == getCurrency a2)

instance HasCurrency (Account t) where
  getCurrency (CAccount {..}) = creditAccountCurrency
  getCurrency (DAccount {..}) = debitAccountCurrency
  getCurrency (FAccount {..}) = freeAccountCurrency

class Named a where
  getName :: a -> String

instance Named (Account t) where
  getName (CAccount {..}) = creditAccountName
  getName (DAccount {..}) = debitAccountName
  getName (FAccount {..}) = freeAccountName

instance (Named (f Free), Named (f t)) => Named (FreeOr t f) where
  getName (Left x) = getName x
  getName (Right x) = getName x

instance Show (Account t) where
  show x = getName x ++ " (" ++ getCurrency x ++ ")"

class CanCredit t where
  credit :: Account t -> Ext (Entry Credit) -> Account t

class CanDebit t where
  debit :: Account t -> Ext (Entry Debit) -> Account t

data AnyAccount =
    WFree   Attributes (Account Free)
  | WCredit Attributes (Account Credit)
  | WDebit  Attributes (Account Debit)
  deriving (Eq)

instance Show AnyAccount where
  show (WFree   attrs x) = "free: "   ++ show x ++ " " ++ showA attrs
  show (WCredit attrs x) = "credit: " ++ show x ++ " " ++ showA attrs
  show (WDebit  attrs x) = "debit: "  ++ show x ++ " " ++ showA attrs

instance Named AnyAccount where
  getName (WFree _ x) = getName x
  getName (WCredit _ x) = getName x
  getName (WDebit _ x)  = getName x

instance HasCurrency AnyAccount where
  getCurrency (WCredit _ a) = getCurrency a
  getCurrency (WDebit _ a)  = getCurrency a
  getCurrency (WFree _ a)   = getCurrency a

accountType :: AnyAccount -> AccountGroupType
accountType (WCredit _ _) = AGCredit
accountType (WDebit  _ _) = AGDebit
accountType (WFree   _ _) = AGFree

accountAttributes :: AnyAccount -> Attributes
accountAttributes (WCredit as _) = as
accountAttributes (WDebit  as _) = as
accountAttributes (WFree   as _) = as

data Query = Query {
    qStart :: Maybe DateTime,
    qEnd   :: Maybe DateTime,
    qAttributes :: Attributes }
  deriving (Eq, Show)

data AccountGroupType =
    AGCredit
  | AGDebit
  | AGFree
  deriving (Eq)

instance Show AccountGroupType where
  show AGCredit = "credit"
  show AGDebit  = "debit"
  show AGFree   = "free"

data AccountGroupData = AccountGroupData {
    agName :: String,
    agCurrency :: Currency,
    agType :: AccountGroupType,
    agAttributes :: Attributes }
  deriving (Eq)

instance Show AccountGroupData where
  show ag = show (agType ag) ++ ": " ++ show (agName ag) ++
            " (" ++ agCurrency ag ++ ") " ++ showA (agAttributes ag)

type AccountPlan = Tree AccountGroupData AnyAccount

