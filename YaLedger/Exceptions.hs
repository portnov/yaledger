{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, DeriveDataTypeable, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Exceptions where

import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc
import Data.List (intercalate)

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
      "No conversion rate defined to convert " ++ c1 ++ " -> " ++ c2

instance Exception NoSuchRate

data InvalidAccountType =
    InvalidAccountType AccountGroupType AccountGroupType SourcePos
  deriving (Typeable)

instance Show InvalidAccountType where
  show (InvalidAccountType t1 t2 pos) = 
    showPos pos $
      "Internal error:\n    Invalid account type: " ++ show t1 ++ " instead of " ++ show t2

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

data InvalidPath = InvalidPath Path [AccountPlan] SourcePos
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

wrapE :: (Monad m, Throws InternalError l)
      => EMT (Caught SomeException (Caught FailException l)) m a
      -> EMT l m a
wrapE action = wrapException wrapFail $ wrapException wrapSome action
  where
    nowhere = newPos "<nowhere>" 0 0
    wrapFail (FailException msg) = InternalError msg nowhere
    wrapSome (SomeException e)   = InternalError (show e) nowhere

