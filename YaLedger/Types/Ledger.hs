{-# LANGUAGE EmptyDataDecls, GADTs, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeSynonymInstances, DeriveDataTypeable, RecordWildCards, ScopedTypeVariables, MultiParamTypeClasses #-}

module YaLedger.Types.Ledger where

import Data.Decimal
import Data.List
import Text.Printf

import YaLedger.Tree
import YaLedger.Attributes
import YaLedger.Types.Common

data Posting v t where
  -- Debit posting
  DPosting :: {
    -- Debit posting should use debit or free account
    debitPostingAccount :: FreeOr Debit Account,
    debitPostingAmount :: v
  } -> Posting v Debit

  CPosting :: {
    -- Credit posting should use credit or free account
    creditPostingAccount :: FreeOr Credit Account,
    creditPostingAmount  :: v
  } -> Posting v Credit

instance Eq v => Eq (Posting v Debit) where
  (DPosting a1 x1) == (DPosting a2 x2) = (a1 == a2) && (x1 == x2)

instance Eq v => Eq (Posting v Credit) where
  (CPosting a1 x1) == (CPosting a2 x2) = (a1 == a2) && (x1 == x2)

instance Show v => Show (Posting v t) where
  show (DPosting acc x) = "dr " ++ showFA acc ++ " " ++ show x
  show (CPosting acc x) = "cr " ++ showFA acc ++ " " ++ show x

showFA :: FreeOr t Account -> String
showFA (Left a) = show a
showFA (Right a) = show a

instance HasCurrency (Posting Amount t) where
  getCurrency (DPosting _ (_ :# c)) = c
  getCurrency (CPosting _ (_ :# c)) = c

instance HasCurrency (Posting Decimal t) where
  getCurrency (DPosting acc _) = getCurrency acc
  getCurrency (CPosting acc _) = getCurrency acc

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

instance HasAmount (Posting Decimal t) where
  getAmount (DPosting acc x) = x :# getCurrency acc
  getAmount (CPosting acc x) = x :# getCurrency acc

data RatesDifference =
    OneCurrency
  | CreditDifference (Posting Decimal Credit)
  | DebitDifference (Posting Decimal Debit)
  deriving (Eq)

instance Show RatesDifference where
  show OneCurrency = "no rates difference"
  show (CreditDifference p) = show p
  show (DebitDifference p) = show p

data Entry v c where
    CEntry :: {
      cEntryDebitPostings  :: [Posting Decimal Debit],
      cEntryCreditPostings :: [Posting Decimal Credit],
      cEntryRatesDifference :: RatesDifference
    } -> Entry Decimal Checked

    UEntry :: {
      uEntryDebitPostings  :: [Posting v Debit],
      uEntryCreditPostings :: [Posting v Credit], 
      uEntryCorrespondence :: Maybe AnyAccount,
      uEntryAdditionalCurrencies :: [Currency]
    } -> Entry v Unchecked

instance Eq v => Eq (Entry v Checked) where
  (CEntry dt cr rd) == (CEntry dt' cr' rd') =
      (dt == dt') && (cr == cr') && (rd == rd')

instance Eq v => Eq (Entry v Unchecked) where
  (UEntry dt cr c cs) == (UEntry dt' cr' c' cs') =
    (dt == dt') && (cr == cr') && (c == c') && (cs == cs')

instance Show v => Show (Entry v t) where
  show (CEntry dt cr rd) =
      "Debit:\n" ++ go dt ++ "Credit:\n" ++ go cr ++ "Rates difference: " ++ show rd
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

type History f t = IOList (Ext (f t))

data Account t where
  CAccount :: {
    creditAccountName     :: String,
    creditAccountID       :: AccountID,
    creditAccountCurrency :: Currency,
    creditAccountEntries  :: History (Entry Decimal) Checked,
    creditAccountPostings :: History (Posting Decimal) Credit
  } -> Account Credit

  DAccount :: {
    debitAccountName     :: String,
    debitAccountID       :: AccountID,
    debitAccountCurrency :: Currency,
    debitAccountEntries  :: History (Entry Decimal) Checked,
    debitAccountPostings :: History (Posting Decimal) Debit
  } -> Account Debit

  FAccount :: {
    freeAccountName           :: String,
    freeAccountID             :: AccountID,
    freeAccountCurrency       :: Currency,
    freeAccountEntries        :: History (Entry Decimal) Checked,
    freeAccountCreditPostings :: History (Posting Decimal) Credit,
    freeAccountDebitPostings  :: History (Posting Decimal) Debit
  } -> Account Free

class HasEntries a where
  accountEntries :: a -> History (Entry Decimal) Checked

instance HasEntries (Account t) where
  accountEntries (CAccount {..}) = creditAccountEntries
  accountEntries (DAccount {..}) = debitAccountEntries
  accountEntries (FAccount {..}) = freeAccountEntries

instance HasEntries (FreeOr t Account) where
  accountEntries (Left a)  = accountEntries a
  accountEntries (Right a) = accountEntries a

instance HasEntries AnyAccount where
  accountEntries (WCredit _ a) = accountEntries a
  accountEntries (WDebit  _ a) = accountEntries a
  accountEntries (WFree   _ a) = accountEntries a

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
    printf "%s (%s)"
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

type AccountPlan = Tree AccountGroupData AnyAccount

data CQuery = CQuery {
  cqType :: PostingType,
  cqCurrency :: [Currency],
  cqExcept :: [AccountID],
  cqAttributes :: Attributes }
  deriving (Eq)

instance Show CQuery where
  show (CQuery {..}) =
    printf "{\n  Type = %s\n  Currencies = %s\n  Except accounts: %s\n  Attributes: %s\n}"
      (show cqType)
      (intercalate ", " cqCurrency)
      (show cqExcept)
      (showA cqAttributes)

