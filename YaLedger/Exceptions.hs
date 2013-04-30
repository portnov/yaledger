{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, DeriveDataTypeable, MultiParamTypeClasses, UndecidableInstances #-}
-- | Declaration of exceptions types
module YaLedger.Exceptions
  (throwP,
   InternalError (..), NoSuchRate (..),
   InsufficientFunds (..), ReconciliationError (..),
   DuplicatedRecord (..), NoSuchHold (..), InvalidAccountType (..),
   NoCorrespondingAccountFound (..), NoSuchTemplate (..),
   InvalidCmdLine (..), InvalidPath (..), NotAnAccount (..)
  ) where

import Control.Monad.Exception
import Control.Monad.State
import Data.List (intercalate)
import Data.Decimal

import YaLedger.Types (Path)
import YaLedger.Types.Ledger
import YaLedger.Types.Common
import YaLedger.Types.Monad

showPos :: SourcePos -> String -> String
showPos pos msg =
  msg ++ "\n  at " ++ show pos

throwP e = do
  pos <- gets lsPosition
  throw (e pos)

data InternalError = InternalError String SourcePos
  deriving (Typeable)

instance Show InternalError where
  show (InternalError msg pos) =
    showPos pos $ "Internal error: " ++ msg

instance Exception InternalError

instance UncaughtException InternalError

data NoSuchRate = NoSuchRate Currency Currency SourcePos
  deriving (Typeable)

instance Show NoSuchRate where
  show (NoSuchRate c1 c2 pos) =
    showPos pos $
      "No conversion rate defined to convert " ++ show c1 ++ " -> " ++ show c2

instance Exception NoSuchRate

data InsufficientFunds = InsufficientFunds String Decimal Currency SourcePos
  deriving (Typeable)

instance Show InsufficientFunds where
  show (InsufficientFunds account x c pos) =
    showPos pos $ 
      "Insufficient funds on account " ++ account ++ ": balance would be " ++ show (x :# c)

instance Exception InsufficientFunds

data ReconciliationError = ReconciliationError String SourcePos
  deriving (Typeable)

instance Show ReconciliationError where
  show (ReconciliationError msg pos) =
    showPos pos $
      "Reconciliation error: " ++ msg

instance Exception ReconciliationError

data NoSuchHold = NoSuchHold PostingType Decimal Path SourcePos
  deriving (Typeable)

instance Show NoSuchHold where
  show (NoSuchHold ptype amt path pos) =
    showPos pos $
      "There is no " ++ show ptype ++ " hold of amount " ++ show amt ++ " on account " ++ intercalate "/" path

instance Exception NoSuchHold

data DuplicatedRecord = DuplicatedRecord String SourcePos
  deriving (Typeable)

instance Show DuplicatedRecord where
  show (DuplicatedRecord s pos) =
    showPos pos $
      "Duplicated records:\n" ++ s

instance Exception DuplicatedRecord

data InvalidAccountType =
    InvalidAccountType String Decimal AccountGroupType AccountGroupType SourcePos
  deriving (Typeable)

instance Show InvalidAccountType where
  show (InvalidAccountType name amt t1 t2 pos) = 
    showPos pos $
      "Internal error:\n    Invalid account type: "
       ++ name ++ ": "
       ++ show t1 ++ " instead of " ++ show t2
       ++ " (amount: " ++ show amt ++ ")"

instance Exception InvalidAccountType

data NoCorrespondingAccountFound =
    NoCorrespondingAccountFound (Delta Amount) CQuery SourcePos
  deriving (Typeable)

instance Show NoCorrespondingAccountFound where
  show (NoCorrespondingAccountFound delta qry pos) =
    showPos pos $ "No corresponding account found by query: " ++ show qry ++
                  "\nwhile need to change some balance: " ++ show delta

instance Exception NoCorrespondingAccountFound

data NoSuchTemplate = NoSuchTemplate String SourcePos
  deriving (Typeable)

instance Show NoSuchTemplate where
  show (NoSuchTemplate name pos) =
    showPos pos $ "No such template was defined: " ++ name

instance Exception NoSuchTemplate

data InvalidCmdLine = InvalidCmdLine String
  deriving (Typeable)

instance Show InvalidCmdLine where
  show (InvalidCmdLine e) =
    "Invalid command line parameter: " ++ e

instance Exception InvalidCmdLine

data InvalidPath = InvalidPath Path [ChartOfAccounts] SourcePos
  deriving (Typeable)

instance Show InvalidPath where
  show (InvalidPath path [] pos) =
    showPos pos $ "No such account: " ++ intercalate "/" path
  show (InvalidPath path list pos) =
    showPos pos $
      "Ambigous account/group specification: " ++ 
        intercalate "/" path ++
        ". Matching are:\n" ++
        unlines (map show list)

instance Exception InvalidPath

data NotAnAccount = NotAnAccount Path SourcePos
  deriving (Typeable)

instance Show NotAnAccount where
  show (NotAnAccount p pos) =
    showPos pos $ 
      "This is accounts group, not an account: " ++
      intercalate "/" p

instance Exception NotAnAccount

