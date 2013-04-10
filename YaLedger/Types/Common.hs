{-# LANGUAGE EmptyDataDecls, GADTs, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeSynonymInstances, DeriveDataTypeable, RecordWildCards, ScopedTypeVariables, MultiParamTypeClasses #-}

module YaLedger.Types.Common
  (Checked, Unchecked,
   Sign (..),
   Credit, Debit, Free,
   IOList,
   Currency (..), Currencies,
   Rate (..), Rates,
   AccountID, GroupID,
   FreeOr,
   Ext (..), mapExt,
   HasAmount (..), Named (..),
   HasCurrency (..), HasID (..),
   HasAttributes (..),
   Amount (..), Param (..),
   AccountGroupType (..),
   AccountGroupData (..),
   MessageFormat, MessageElement (..),
   SourcePos,
   Queue,
   amountValue,
   absAmount,
   isNotZero,
   emptyCurrency,
   sourceLine, sourceColumn, sourceName,
   newPos, nowhere
  ) where

import Control.Concurrent.STM
import qualified Data.PQueue.Prio.Min as Q
import Data.Decimal
import Data.Dates
import qualified Data.Map as M
import Data.Hashable
import Text.Printf
import Text.Parsec.Pos

import YaLedger.Types.Attributes

data Checked
data Unchecked

data Credit
data Debit
data Free

class Sign t where
  sign :: t -> Int

instance Sign Credit where
  sign _ = 1

instance Sign Debit where
  sign _ = -1

instance Sign Free where
  sign _ = 0

instance Sign t => Sign (f t) where
  sign _ = sign (undefined :: t)

type IOList a = TVar [a]

data Currency =
  Currency {
    cSymbol :: String,
    cIntCode :: Maybe Int,
    cStrCode :: Maybe String,
    cPrecision :: Int }
  deriving (Eq)

instance Show Currency where
  show = cSymbol

instance Ord Currency where
  compare c1 c2 = compare (cSymbol c1) (cSymbol c2)

instance Hashable Currency where
  hashWithSalt s c =
           s `hashWithSalt`
           (cSymbol c) `hashWithSalt`
           (cIntCode c) `hashWithSalt`
           (cStrCode c) `hashWithSalt`
           (cPrecision c)

emptyCurrency :: Currency
emptyCurrency = Currency {
  cSymbol = "¤",
  cIntCode = Nothing,
  cStrCode = Nothing,
  cPrecision = 2 }

type Currencies = M.Map String Currency

data Rate =
    Explicit {
      rateCurrencyFrom :: Currency,
      rateAmountFrom   :: Double,
      rateCurrencyTo   :: Currency,
      rateAmountTo     :: Double,
      rateReversible   :: Bool }
  | Implicit {
      rateCurrencyFrom :: Currency,
      rateCurrencyTo   :: Currency,
      rateBaseCurrency :: Currency,
      rateReversible   :: Bool }
  deriving (Eq, Show)

type Rates = [Ext Rate]

type AccountID = Integer

type GroupID = Integer

type FreeOr t f = Either (f Free) (f t)

data Ext a = Ext {
    getDate :: DateTime,
    extID   :: Integer,
    getLocation :: SourcePos,
    getAttributes :: Attributes,
    getContent :: a }
  deriving (Eq, Show)

mapExt :: (a -> b) -> Ext a -> Ext b
mapExt fn (Ext {..}) =
  Ext {
    getDate = getDate,
    extID = extID,
    getLocation = getLocation,
    getAttributes = getAttributes,
    getContent = fn getContent }

nowhere :: SourcePos
nowhere = newPos "<nowhere>" 0 0

class HasAmount a where
  getAmount :: a -> Amount

class Named a where
  getName :: a -> String

class HasID a where
  getID :: a -> Integer

class HasCurrency a where
  getCurrency :: a -> Currency

class HasAttributes a where
  getAttrs :: a -> Attributes

data Amount = Decimal :# Currency
  deriving (Eq)

-- | Get value of amount.
-- E.g., amountValue (5$) = 5.
amountValue :: Amount -> Decimal
amountValue (x :# _) = x

-- | Get absolute value of Amount.
-- E.g., absAmount (5$) = -5$.
absAmount :: Amount -> Amount
absAmount (x :# c) = abs x :# c

-- | Check if amount is not zero.
isNotZero :: Amount -> Bool
isNotZero (x :# _) = x /= 0

instance Show Amount where
  show (n :# c) = show (roundTo (fromIntegral $ cPrecision c) n) ++ show c

instance Hashable Amount where
  hashWithSalt s (n :# c) = s `hashWithSalt` n `hashWithSalt` c

instance Hashable Decimal where
  hashWithSalt s (Decimal n x) = s `hashWithSalt` x `hashWithSalt` n

instance HasCurrency Amount where
  getCurrency (_ :# c) = c

data Param =
    Fixed Amount
  | Param Int Double Amount
  | FromBalance Double
  | Plus Param Param
  deriving (Eq)

instance Show Param where
  show (Fixed x) = show x
  show (Param n x d) = "#" ++ show n ++ " * " ++ show x
                    ++ " (default " ++ show d ++ ")"
  show (FromBalance c) = "#balance*" ++ show c
  show (Plus x y) = show x ++ " + " ++ show y

instance Eq a => Ord (Ext a) where
  compare x y =
    case compare (getDate x) (getDate y) of
      EQ -> compare (getLocation x) (getLocation y)
      c  -> c

instance HasID a => HasID (Ext a) where
  getID x = getID (getContent x)

instance Named a => Named (Ext a) where
  getName x = getName (getContent x)

instance (HasID (f Free), HasID (f t)) => HasID (FreeOr t f) where
  getID (Left x)  = getID x
  getID (Right x) = getID x

instance HasAmount a => HasAmount (Ext a) where
  getAmount x = getAmount (getContent x)

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
      (show $ agCurrency ag)
      (fst $ agRange ag)
      (snd $ agRange ag)
      (showA $ agAttributes ag)

instance HasID AccountGroupData where
  getID ag = agID ag

data MessageElement =
    MVariable String
  | MFixed String
  deriving (Eq, Show)

type MessageFormat = [MessageElement]

type Queue a = TVar (Q.MinPQueue (DateTime, Integer) (Ext a))

