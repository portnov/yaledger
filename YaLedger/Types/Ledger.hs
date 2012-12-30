{-# LANGUAGE EmptyDataDecls, GADTs, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeSynonymInstances, DeriveDataTypeable, RecordWildCards, ScopedTypeVariables, MultiParamTypeClasses, StandaloneDeriving #-}

module YaLedger.Types.Ledger where

import Control.Applicative
import qualified Data.Map as M
import Data.Decimal
import Data.List
import Data.Hashable
import Text.Printf
import Data.Dates

import YaLedger.Tree
import YaLedger.Types.Attributes
import YaLedger.Types.Common

data Posting v t where
  -- Debit posting
  DPosting :: {
    -- Debit posting should use debit or free account
    debitPostingAccount :: FreeOr Debit Account,
    debitPostingAmount  :: v,
    debitPostingUseHold :: Bool
  } -> Posting v Debit

  CPosting :: {
    -- Credit posting should use credit or free account
    creditPostingAccount :: FreeOr Credit Account,
    creditPostingAmount  :: v,
    creditPostingUseHold :: Bool
  } -> Posting v Credit

instance Eq v => Eq (Posting v Debit) where
  (DPosting a1 x1 b1) == (DPosting a2 x2 b2) = (a1 == a2) && (x1 == x2) && (b1 == b2)

instance Eq v => Eq (Posting v Credit) where
  (CPosting a1 x1 b1) == (CPosting a2 x2 b2) = (a1 == a2) && (x1 == x2) && (b1 == b2)

instance Show v => Show (Posting v t) where
  show (DPosting acc x b) = "dr " ++ showFA acc ++ " " ++ show x ++ if b then " (use hold)" else ""
  show (CPosting acc x b) = "cr " ++ showFA acc ++ " " ++ show x ++ if b then " (use hold)" else ""

instance Hashable v => Hashable (Posting v t) where
  hashWithSalt s (DPosting acc x b) = s `hashWithSalt` (1000000*getID acc) `hashWithSalt` x `hashWithSalt` b
  hashWithSalt s (CPosting acc x b) = s `hashWithSalt` (2000000*getID acc) `hashWithSalt` x `hashWithSalt` b

postingValue :: Posting v t -> v
postingValue (DPosting {..}) = debitPostingAmount
postingValue (CPosting {..}) = creditPostingAmount

postingAccount :: Posting v t -> AnyAccount
postingAccount (DPosting {..}) = either (WFree M.empty) (WDebit M.empty) debitPostingAccount
postingAccount (CPosting {..}) = either (WFree M.empty) (WCredit M.empty) creditPostingAccount

postingAccount' :: Posting v t -> FreeOr t Account
postingAccount' (DPosting acc _ _) = acc
postingAccount' (CPosting acc _ _) = acc

showFA :: FreeOr t Account -> String
showFA (Left a) = show a
showFA (Right a) = show a

instance HasCurrency (Posting Amount t) where
  getCurrency (DPosting _ (_ :# c) _) = c
  getCurrency (CPosting _ (_ :# c) _) = c

instance HasCurrency (Posting Decimal t) where
  getCurrency (DPosting acc _ _) = getCurrency acc
  getCurrency (CPosting acc _ _) = getCurrency acc

data Hold v t = Hold {
    holdPosting :: Posting v t
  , holdEndDate :: Maybe DateTime }

deriving instance Eq (Posting v t) => Eq (Hold v t)

instance HasCurrency (Hold Amount t) where
  getCurrency (Hold p _) = getCurrency p

instance HasCurrency (Hold Decimal t) where
  getCurrency (Hold p _) = getCurrency p

instance Show v => Show (Hold v t) where
  show (Hold p _) = "hold " ++ show p

data PostingType =
    EDebit
  | ECredit
  deriving (Eq)

instance Show PostingType where
  show EDebit  = "debit"
  show ECredit = "credit"

instance HasAmount (Posting Amount t) where
  getAmount (DPosting _ x _) = x
  getAmount (CPosting _ x _) = x

instance HasAmount (Posting Decimal t) where
  getAmount (DPosting acc x _) = x :# getCurrency acc
  getAmount (CPosting acc x _) = x :# getCurrency acc

instance (HasAmount a, HasAmount b) => HasAmount (Either a b) where
  getAmount (Left x)  = getAmount x
  getAmount (Right x) = getAmount x

data RatesDifference =
    OneCurrency
  | CreditDifference (Posting Decimal Credit)
  | DebitDifference [Posting Decimal Debit]
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
                              ++ "(add. currencies: " ++ intercalate ", " (map show cs) ++ ")"
    where
      go :: Show a => [a] -> String
      go lst = unlines $ map ("  " ++) $ map show lst

      showName Nothing = "to be found automatically"
      showName (Just x) = getName x

-- | Historical data
type History f t = IOList (Ext (f t))

-- | Item of account balances history
data Balance c =
  Balance {
    causedBy     :: Maybe (Entry Decimal c)
  , balanceValue :: Decimal  -- ^ Balance value itself (sum of postings)
  , creditHolds  :: Decimal  -- ^ Sum of all credit holds
  , debitHolds   :: Decimal  -- ^ Sum of all debit holds
  }

deriving instance Eq (Entry Decimal c) => Eq (Balance c)

instance Show (Balance c) where
  show b = show (balanceValue b)

-- | Balance checks for each posting for one account
data BalanceChecks =
  BalanceChecks {
    bcInfo    :: Maybe Decimal,
    bcWarning :: Maybe Decimal,
    bcError   :: Maybe Decimal }
  deriving (Eq, Show)

data BalanceType =
    LedgerBalance
  | AvailableBalance
  deriving (Eq, Show)

-- | Which balances to query.
data BalanceQuery =
    Only BalanceType  -- ^ only specified balance
  | BothBalances      -- ^ both available and ledger balance
  deriving (Eq, Show)

-- | Return value of balance querying functions
data BalanceInfo v = BalanceInfo {
    biAvailable :: Maybe v  -- ^ Available balance, if queried
  , biLedger :: Maybe v     -- ^ Ledger balance, if queried
  }
  deriving (Eq)

instance (Eq v, Show v) => Show (BalanceInfo v) where
  show (BalanceInfo Nothing Nothing) = "NA"
  show (BalanceInfo (Just x) Nothing) = show x
  show (BalanceInfo Nothing (Just x)) = show x
  show (BalanceInfo (Just a) (Just l))
    | a == l = show a
    | otherwise = show a ++ " / " ++ show l

-- | Generic account type.
data Account t where
  CAccount :: {
    creditAccountName     :: String,
    creditAccountID       :: AccountID,
    creditAccountCurrency :: Currency,
    -- | For credit accounts, we'll check if balance is GREATER THAN given values
    creditAccountChecks   :: BalanceChecks,
    creditAccountBalances :: History Balance Checked,
    creditAccountHolds    :: History (Hold Decimal) Credit,
    creditAccountPostings :: History (Posting Decimal) Credit
  } -> Account Credit

  DAccount :: {
    debitAccountName     :: String,
    debitAccountID       :: AccountID,
    debitAccountCurrency :: Currency,
    debitAccountChecks   :: BalanceChecks,
    debitAccountBalances :: History Balance Checked,
    debitAccountHolds    :: History (Hold Decimal) Debit,
    debitAccountPostings :: History (Posting Decimal) Debit
  } -> Account Debit

  FAccount :: {
    freeAccountName           :: String,
    freeAccountID             :: AccountID,
    freeAccountCurrency       :: Currency,
    -- | Whenever to partially redirect debits when balance is too small.
    freeAccountRedirect       :: Bool,
    freeAccountChecks         :: BalanceChecks,
    freeAccountBalances       :: History Balance Checked,
    freeAccountCreditHolds    :: History (Hold Decimal) Credit,
    freeAccountDebitHolds     :: History (Hold Decimal) Debit,
    freeAccountCreditPostings :: History (Posting Decimal) Credit,
    freeAccountDebitPostings  :: History (Posting Decimal) Debit
  } -> Account Free 

class HasBalances a where
  accountBalances :: a -> History Balance Checked
  accountChecks :: a -> BalanceChecks

instance HasBalances (Account t) where
  accountBalances (CAccount {..}) = creditAccountBalances
  accountBalances (DAccount {..}) = debitAccountBalances
  accountBalances (FAccount {..}) = freeAccountBalances

  accountChecks (CAccount {..}) = creditAccountChecks
  accountChecks (DAccount {..}) = debitAccountChecks
  accountChecks (FAccount {..}) = freeAccountChecks

instance HasBalances (FreeOr t Account) where
  accountBalances (Left a)  = accountBalances a
  accountBalances (Right a) = accountBalances a

  accountChecks (Left a)  = accountChecks a
  accountChecks (Right a) = accountChecks a

instance HasBalances AnyAccount where
  accountBalances (WCredit _ a) = accountBalances a
  accountBalances (WDebit  _ a) = accountBalances a
  accountBalances (WFree   _ a) = accountBalances a

  accountChecks (WCredit _ a) = accountChecks a
  accountChecks (WDebit  _ a) = accountChecks a
  accountChecks (WFree   _ a) = accountChecks a

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
      (show $ getCurrency x)

data AnyAccount =
    WFree   Attributes (Account Free)
  | WCredit Attributes (Account Credit)
  | WDebit  Attributes (Account Debit)
  deriving (Eq)

instance Show AnyAccount where
  show (WFree   attrs x) = "free: "   ++ show (getID x) ++ ": " ++ show x ++ " " ++ showA attrs
  show (WCredit attrs x) = "credit: " ++ show (getID x) ++ ": " ++ show x ++ " " ++ showA attrs
  show (WDebit  attrs x) = "debit: "  ++ show (getID x) ++ ": " ++ show x ++ " " ++ showA attrs

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

class (Named a, HasBalances a, HasCurrency a, Sign a) => IsAccount a 

instance (Named a, HasBalances a, HasCurrency a, Sign a) => IsAccount a 

-- | Chart of accounts is a tree of accounts and account groups
type ChartOfAccounts = Tree AccountGroupData AnyAccount

-- | Query to search for a corresponding account
data CQuery = CQuery {
  cqType :: PostingType,
  cqCurrencies :: [Currency],
  cqExcept :: [AccountID],
  cqAttributes :: Attributes }
  deriving (Eq)

instance Show CQuery where
  show (CQuery {..}) =
    printf "{\n  Type = %s\n  Currencies = %s\n  Except accounts: %s\n  Attributes: %s\n}"
      (show cqType)
      (intercalate ", " $ map show cqCurrencies)
      (show cqExcept)
      (showA cqAttributes)

