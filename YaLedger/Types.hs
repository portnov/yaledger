{-# LANGUAGE EmptyDataDecls, GADTs, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeSynonymInstances, DeriveDataTypeable, RecordWildCards, ScopedTypeVariables, MultiParamTypeClasses #-}

module YaLedger.Types where

import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc
import Control.Monad.Trans
import Data.List
import Data.Dates
import Data.Decimal
import qualified Data.Map as M
import Text.Regex.PCRE
import Text.Printf

import YaLedger.Tree

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

data Ext a = Ext {
    getDate :: DateTime,
    getAttributes :: Attributes,
    getContent :: a }
  deriving (Eq, Show)

class HasAmount a v where
  getAmount :: a -> Amount v

class Named a where
  getName :: a -> String

class HasID a where
  getID :: a -> Integer

class HasCurrency a where
  getCurrency :: a -> Currency

class CanCredit t where
  credit :: Account t -> Ext (Posting Decimal Credit) -> Account t

class CanDebit t where
  debit :: Account t -> Ext (Posting Decimal Debit) -> Account t

class AmountKind v where
  zero :: v
  plus :: v -> v -> v
  minus :: v -> v -> v
  neg :: v -> v
  multiply :: v -> Double -> v

data Param =
    Fixed Decimal
  | Param Int Double Decimal
  | Plus Param Param
  deriving (Eq)

instance Show Param where
  show (Fixed x) = show x
  show (Param n x d) = "#" ++ show n ++ " * " ++ show x
                    ++ " (default " ++ show d ++ ")"
  show (Plus x y) = show x ++ " + " ++ show y

instance AmountKind Param where
  zero = Fixed 0

  Fixed x `plus` Fixed y = Fixed (x+y)
  x@(Param n1 c1 d1) `plus` y@(Param n2 c2 d2)
    | n1 == n2  = Param n1 (c1+c2) (d1+d2)
    | otherwise = Plus x y
  Plus x (Fixed y) `plus` Fixed z = Plus x (Fixed (y+z))
  Plus x y `plus` z = Plus x (Plus y z)
  x `plus` y = Plus x y

  neg (Fixed x) = Fixed (-x)
  neg (Param n c d) = Param n (-c) d
  neg (Plus x y) = Plus (neg x) (neg y)

  Fixed x `minus` Fixed y = Fixed (x-y)
  x@(Param n1 c1 d1) `minus` y@(Param n2 c2 d2)
    | n1 == n2  = Param n1 (c1-c2) (d1-d2)
    | otherwise = Plus x (neg y)
  Plus x (Fixed y) `minus` Fixed z = Plus x (Fixed (y-z))
  Plus x y `minus` z = Plus x (Plus y (neg z))
  x `minus` y = Plus x (neg y)

  Fixed x `multiply` c = Fixed (x *. c)
  Param n c d `multiply` x = Param n (c*x) (d *. x)
  Plus x y `multiply` c = Plus (x `multiply` c) (y `multiply` c)

instance AmountKind Decimal where
  zero = 0
  plus = (+)
  minus = (-)
  neg = negate
  multiply = (*.)

instance Eq a => Ord (Ext a) where
  compare x y = compare (getDate x) (getDate y)

instance HasID a => HasID (Ext a) where
  getID x = getID (getContent x)

instance Named a => Named (Ext a) where
  getName x = getName (getContent x)

data Record =
    Template String (Transaction Param)
  | Transaction (Transaction Decimal)
  deriving (Show)

data Transaction v =
    TEntry (Entry v Unchecked)
  | TReconcilate Path (Amount v)
  | TInitlalize  Path (Amount v)
  deriving (Eq, Show)

data Entry v c where
    CEntry :: {
      cEntryDebitPostings :: [Posting v Debit],
      cEntryCreditPostings :: [Posting v Credit]
    } -> Entry v Checked

    UEntry :: {
      uEntryDebitPostings :: [Posting v Debit],
      uEntryCreditPostings :: [Posting v Credit],
      uEntryCorrespondence :: Maybe AnyAccount
    } -> Entry v Unchecked

instance Eq v => Eq (Entry v Checked) where
  (CEntry dt cr) == (CEntry dt' cr') = (dt == dt') && (cr == cr')

instance Eq v => Eq (Entry v Unchecked) where
  (UEntry dt cr c) == (UEntry dt' cr' c') = (dt == dt') && (cr == cr') && (c == c')

instance Show v => Show (Entry v t) where
  show (CEntry dt cr) = "Debit:\n" ++ go dt ++ "\nCredit:\n" ++ go cr
    where
      go lst = unlines $ map ("  " ++) $ map show lst
  show (UEntry dt cr acc) = "Debit:\n" ++ go dt ++ "\nCredit:\n" ++ go cr
                              ++ "(correspondence: " ++ showName acc ++ ")"
    where
      go lst = unlines $ map ("  " ++) $ map show lst

      showName Nothing = "to be found automatically"
      showName (Just x) = getName x

data Amount v = v :# Currency
  deriving (Eq)

instance Show v => Show (Amount v) where
  show (n :# c) = show n ++ c

type Currency = String

type Rates = M.Map (Currency, Currency) Double

type FreeOr t f = Either (f Free) (f t)

instance (HasID (f Free), HasID (f t)) => HasID (FreeOr t f) where
  getID (Left x)  = getID x
  getID (Right x) = getID x

data Posting v t where
  DPosting :: {
    debitPostingAccount :: FreeOr Debit Account,
    debitPostingAmount :: Amount v
  } -> Posting v Debit

  CPosting :: {
    creditPostingAccount :: FreeOr Credit Account,
    creditPostingAmount  :: Amount v
  } -> Posting v Credit

instance Eq v => Eq (Posting v Debit) where
  (DPosting a1 x1) == (DPosting a2 x2) = (a1 == a2) && (x1 == x2)

instance Eq v => Eq (Posting v Credit) where
  (CPosting a1 x1) == (CPosting a2 x2) = (a1 == a2) && (x1 == x2)

instance Show v => Show (Posting v t) where
  show (DPosting acc x) = "debit " ++ getName acc ++ " by " ++ show x
  show (CPosting acc x) = "credit " ++ getName acc ++ " by " ++ show x

instance HasCurrency (Posting v Debit) where
  getCurrency (DPosting _ (_ :# c)) = c

instance HasCurrency (Posting v Credit) where
  getCurrency (CPosting _ (_ :# c)) = c

data PostingType =
    EDebit
  | ECredit
  deriving (Eq)

instance Show PostingType where
  show EDebit  = "debit"
  show ECredit = "credit"

instance HasAmount (Posting v t) v where
  getAmount (DPosting _ x) = x
  getAmount (CPosting _ x) = x

instance HasAmount a v => HasAmount (Ext a) v where
  getAmount x = getAmount (getContent x)

data Account t where
  CAccount :: {
    creditAccountName :: String,
    creditAccountID :: Integer,
    creditAccountCurrency :: Currency,
    creditAccountPostings :: [Ext (Posting Decimal Credit)]
  } -> Account Credit

  DAccount :: {
    debitAccountName :: String,
    debitAccountID :: Integer,
    debitAccountCurrency :: Currency,
    debitAccountPostings :: [Ext (Posting Decimal Debit)]
  } -> Account Debit

  FAccount :: {
    freeAccountName :: String,
    freeAccountID :: Integer,
    freeAccountCurrency :: Currency,
    freeAccountCreditPostings :: [Ext (Posting Decimal Credit)],
    freeAccountDebitPostings :: [Ext (Posting Decimal Debit)]
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

type AccountPlan = Tree AccountGroupData AnyAccount

type AccountMap = [AMEntry]

data AMEntry = AMPointer :=> AccountPlan
  deriving (Eq)

instance Show AMEntry where
  show (ptr :=> tgt) = show ptr ++ " maps to:\n" ++ show tgt

data AMPointer =
    AMAccount Integer
  | AMGroup Integer
  deriving (Eq)

instance Show AMPointer where
  show (AMAccount i) = "account #" ++ show i
  show (AMGroup i)   = "group #"   ++ show i

