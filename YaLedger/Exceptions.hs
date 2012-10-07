{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, DeriveDataTypeable, MultiParamTypeClasses, UndecidableInstances #-}

module YaLedger.Exceptions where

import Control.Monad.Exception
import Control.Monad.Loc
import Data.List (intercalate)
import Data.Decimal

import YaLedger.Tree
import YaLedger.Types.Ledger
import YaLedger.Types.Common

showPos :: SourcePos -> String -> String
showPos pos msg =
  msg ++ "\n  at " ++ show pos

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

data DuplicatedRecord = DuplicatedRecord String SourcePos
  deriving (Typeable)

instance Show DuplicatedRecord where
  show (DuplicatedRecord s pos) =
    showPos pos $
      "Duplicated records:\n" ++ s

instance Exception DuplicatedRecord

data InvalidAccountType =
    InvalidAccountType String AccountGroupType AccountGroupType SourcePos
  deriving (Typeable)

instance Show InvalidAccountType where
  show (InvalidAccountType name t1 t2 pos) = 
    showPos pos $
      "Internal error:\n    Invalid account type: "
       ++ name ++ ": "
       ++ show t1 ++ " instead of " ++ show t2

instance Exception InvalidAccountType

data NoCorrespondingAccountFound =
    NoCorrespondingAccountFound CQuery SourcePos
  deriving (Typeable)

instance Show NoCorrespondingAccountFound where
  show (NoCorrespondingAccountFound qry pos) =
    showPos pos $ "No corresponding account found by query: " ++ show qry

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

wrapE :: (Monad m, Throws InternalError l)
      => EMT (Caught SomeException (Caught FailException l)) m a
      -> EMT l m a
wrapE action = wrapException wrapFail $ wrapException wrapSome action
  where
    nowhere = newPos "<nowhere>" 0 0
    wrapFail (FailException msg) = InternalError msg nowhere
    wrapSome (SomeException e)   = InternalError (show e) nowhere

