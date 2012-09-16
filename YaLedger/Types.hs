{-# LANGUAGE EmptyDataDecls, GADTs, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeSynonymInstances, DeriveDataTypeable, RecordWildCards, ScopedTypeVariables, MultiParamTypeClasses #-}

module YaLedger.Types
  (Checked, Unchecked,
   Credit, Debit, Free,
   Currency, Rates, FreeOr,
   Ext (..), AccountID, GroupID,
   Account (..), Amount (..),
   AnyAccount (..),
   Posting (..), Entry (..),
   Param (..),
   Transaction (..), Record (..),
   AccountGroupData (..), AccountPlan,
   AccountGroupType (..), PostingType (..),
   AccountMap, AMEntry (..), AMFrom (..), AMTo (..),
   HasCurrency (..),  HasAmount (..), Named (..), HasID (..),
   AccountHistory, Query (..), IOList,
   accountAttributes, accountType,
   module YaLedger.Tree,
   module YaLedger.Attributes
  ) where

import Data.List
import Data.Dates
import Data.Decimal
import Data.IORef
import qualified Data.Map as M
import Text.Printf

import YaLedger.Tree
import YaLedger.Attributes

data Checked
data Unchecked

data Credit
data Debit
data Free

type IOList a = IORef [a]

type Currency = String

type Rates = M.Map (Currency, Currency) Double

type AccountID = Integer

type GroupID = Integer

type FreeOr t f = Either (f Free) (f t)

data Ext a = Ext {
    getDate :: DateTime,
    getAttributes :: Attributes,
    getContent :: a }
  deriving (Eq, Show)

class HasAmount a where
  getAmount :: a -> Amount

class Named a where
  getName :: a -> String

class HasID a where
  getID :: a -> Integer

class HasCurrency a where
  getCurrency :: a -> Currency

data Amount = Decimal :# Currency
  deriving (Eq)

instance Show Amount where
  show (n :# c) = show n ++ c

instance HasCurrency Amount where
  getCurrency (_ :# c) = c

data Param =
    Fixed Amount
  | Param Int Double Amount
  | Plus Param Param
  deriving (Eq)

instance Show Param where
  show (Fixed x) = show x
  show (Param n x d) = "#" ++ show n ++ " * " ++ show x
                    ++ " (default " ++ show d ++ ")"
  show (Plus x y) = show x ++ " + " ++ show y

instance Eq a => Ord (Ext a) where
  compare x y = compare (getDate x) (getDate y)

instance HasID a => HasID (Ext a) where
  getID x = getID (getContent x)

instance Named a => Named (Ext a) where
  getName x = getName (getContent x)

data Record =
    Template String (Transaction Param)
  | Transaction (Transaction Amount)
  deriving (Show)

data Transaction v =
    TEntry (Entry v Unchecked)
  | TReconciliate AnyAccount v
  | TCallTemplate String [Amount]
  | TSetRate Currency Currency Double
  deriving (Eq, Show)

data Posting v t where
  DPosting :: {
    debitPostingAccount :: FreeOr Debit Account,
    debitPostingAmount :: v
  } -> Posting v Debit

  CPosting :: {
    creditPostingAccount :: FreeOr Credit Account,
    creditPostingAmount  :: v
  } -> Posting v Credit

instance Eq v => Eq (Posting v Debit) where
  (DPosting a1 x1) == (DPosting a2 x2) = (a1 == a2) && (x1 == x2)

instance Eq v => Eq (Posting v Credit) where
  (CPosting a1 x1) == (CPosting a2 x2) = (a1 == a2) && (x1 == x2)

instance Show v => Show (Posting v t) where
  show (DPosting acc x) = "debit #" ++ show acc ++ " by " ++ show x
  show (CPosting acc x) = "credit #" ++ show acc ++ " by " ++ show x

instance HasCurrency (Posting Amount t) where
  getCurrency (DPosting _ (_ :# c)) = c
  getCurrency (CPosting _ (_ :# c)) = c

data PostingType =
    EDebit
  | ECredit
  deriving (Eq)

instance Show PostingType where
  show EDebit  = "debit"
  show ECredit = "credit"

instance HasAmount (Posting Amount t) where
  getAmount (DPosting _ x) = x
  getAmount (CPosting _ x) = x

data Entry v c where
    CEntry :: {
      cEntryDebitPostings  :: [Posting Decimal Debit],
      cEntryCreditPostings :: [Posting Decimal Credit]
    } -> Entry Decimal Checked

    UEntry :: {
      uEntryDebitPostings  :: [Posting v Debit],
      uEntryCreditPostings :: [Posting v Credit],
      uEntryCorrespondence :: Maybe AnyAccount,
      uEntryAdditionalCurrencies :: [Currency]
    } -> Entry v Unchecked

instance Eq v => Eq (Entry v Checked) where
  (CEntry dt cr) == (CEntry dt' cr') = (dt == dt') && (cr == cr')

instance Eq v => Eq (Entry v Unchecked) where
  (UEntry dt cr c cs) == (UEntry dt' cr' c' cs') =
    (dt == dt') && (cr == cr') && (c == c') && (cs == cs')

instance Show v => Show (Entry v t) where
  show (CEntry dt cr) = "Debit:\n" ++ go dt ++ "\nCredit:\n" ++ go cr
    where
      go :: Show a => [a] -> String
      go lst = unlines $ map ("  " ++) $ map show lst
  show (UEntry dt cr acc cs) = "Debit:\n" ++ go dt ++ "\nCredit:\n" ++ go cr
                              ++ "(correspondence: " ++ showName acc ++ ")\n"
                              ++ "(add. currencies: " ++ intercalate ", " cs ++ ")"
    where
      go :: Show a => [a] -> String
      go lst = unlines $ map ("  " ++) $ map show lst

      showName Nothing = "to be found automatically"
      showName (Just x) = getName x

instance (HasID (f Free), HasID (f t)) => HasID (FreeOr t f) where
  getID (Left x)  = getID x
  getID (Right x) = getID x

instance HasAmount a => HasAmount (Ext a) where
  getAmount x = getAmount (getContent x)

type AccountHistory t = IOList (Ext (Posting Decimal t))

data Account t where
  CAccount :: {
    creditAccountName     :: String,
    creditAccountID       :: AccountID,
    creditAccountCurrency :: Currency,
    creditAccountPostings :: AccountHistory Credit
  } -> Account Credit

  DAccount :: {
    debitAccountName     :: String,
    debitAccountID       :: AccountID,
    debitAccountCurrency :: Currency,
    debitAccountPostings :: AccountHistory Debit
  } -> Account Debit

  FAccount :: {
    freeAccountName           :: String,
    freeAccountID             :: AccountID,
    freeAccountCurrency       :: Currency,
    freeAccountCreditPostings :: AccountHistory Credit,
    freeAccountDebitPostings  :: AccountHistory Debit
  } -> Account Free

instance HasID (Account t) where
  getID (CAccount {..}) = creditAccountID
  getID (DAccount {..}) = debitAccountID
  getID (FAccount {..}) = freeAccountID

instance Eq (Account t) where
  a1 == a2 = getID a1 == getID a2

instance HasCurrency (Account t) where
  getCurrency (CAccount {..}) = creditAccountCurrency
  getCurrency (DAccount {..}) = debitAccountCurrency
  getCurrency (FAccount {..}) = freeAccountCurrency

instance (HasCurrency a, HasCurrency b) => HasCurrency (Either a b) where
  getCurrency (Left x) = getCurrency x
  getCurrency (Right x) = getCurrency x

instance Named (Account t) where
  getName (CAccount {..}) = creditAccountName
  getName (DAccount {..}) = debitAccountName
  getName (FAccount {..}) = freeAccountName

instance (Named (f Free), Named (f t)) => Named (FreeOr t f) where
  getName (Left x) = getName x
  getName (Right x) = getName x

instance Show (Account t) where
  show x =
    printf "#%d: %s (%s)"
      (getID x)
      (getName x)
      (getCurrency x)

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

instance HasID AnyAccount where
  getID (WCredit _ a) = getID a
  getID (WDebit  _ a) = getID a
  getID (WFree   _ a) = getID a

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
    agID :: Integer,
    agRange :: (Integer, Integer),
    agCurrency :: Currency,
    agType :: AccountGroupType,
    agAttributes :: Attributes }
  deriving (Eq)

instance Show AccountGroupData where
  show ag =
    printf "#%d: %s: %s (%s) (%d--%d] %s"
      (agID ag)
      (show $ agType ag)
      (agName ag)
      (agCurrency ag)
      (fst $ agRange ag)
      (snd $ agRange ag)
      (showA $ agAttributes ag)

type AccountPlan = Tree Linked AccountGroupData AnyAccount

type AccountMap = [AMEntry]

data AMEntry = AMFrom :=> AMTo
  deriving (Eq)

instance Show AMEntry where
  show (ptr :=> tgt) = show ptr ++ " maps to:\n" ++ show tgt

data AMFrom =
    AMAccount AccountID
  | AMGroup GroupID
  | AMAttributes Attributes
  deriving (Eq)

instance Show AMFrom where
  show (AMAccount i) = "account #" ++ show i
  show (AMGroup i)   = "group #"   ++ show i
  show (AMAttributes as) = "attributes " ++ showA as

data AMTo =
    ToAccountPlan AccountPlan
  | ToAttributes Attributes
  deriving (Eq, Show)

