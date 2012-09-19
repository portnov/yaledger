{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, DeriveDataTypeable, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Exceptions where

import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc

import YaLedger.Types
import YaLedger.Correspondence

data InternalError = InternalError String
  deriving (Typeable)

instance Show InternalError where
  show (InternalError msg) = "Internal error: " ++ msg

instance Exception InternalError

instance UncaughtException InternalError

data NoSuchRate = NoSuchRate Currency Currency
  deriving (Typeable)

instance Show NoSuchRate where
  show (NoSuchRate c1 c2) =
    "No conversion rate defined to convert " ++ c1 ++ " -> " ++ c2

instance Exception NoSuchRate

data InvalidAccountType = InvalidAccountType AccountGroupType AccountGroupType
  deriving (Typeable)

instance Show InvalidAccountType where
  show (InvalidAccountType t1 t2) = 
    "Internal error:\n    Invalid account type: " ++ show t1 ++ " instead of " ++ show t2

instance Exception InvalidAccountType

data NoCorrespondingAccountFound =
    NoCorrespondingAccountFound CQuery
  deriving (Typeable)

instance Show NoCorrespondingAccountFound where
  show (NoCorrespondingAccountFound qry) =
    "No corresponding account found by query: " ++ show qry

instance Exception NoCorrespondingAccountFound

data NoSuchTemplate = NoSuchTemplate String
  deriving (Typeable)

instance Show NoSuchTemplate where
  show (NoSuchTemplate name) = "No such template was defined: " ++ name

instance Exception NoSuchTemplate

data InvalidCmdLine = InvalidCmdLine String
  deriving (Typeable)

instance Show InvalidCmdLine where
  show (InvalidCmdLine e) = "Invalid command line parameter: " ++ e

instance Exception InvalidCmdLine

force :: Monad m => EMT (Caught NoSuchRate NoExceptions) m a -> m a
force action = runEMT $ action `catchWithSrcLoc` 
                               \loc (e :: NoSuchRate) -> fail (showExceptionWithTrace loc e)

wrapE :: (Monad m, Throws InternalError l)
      => EMT (Caught SomeException (Caught FailException l)) m a
      -> EMT l m a
wrapE action = wrapException wrapFail $ wrapException wrapSome action
  where
    wrapFail (FailException msg) = InternalError msg
    wrapSome (SomeException e)   = InternalError (show e)

