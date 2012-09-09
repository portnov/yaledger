{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, DeriveDataTypeable #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Exceptions where

import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc

import YaLedger.Types
import YaLedger.Correspondence

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

force :: Monad m => EMT (Caught NoSuchRate NoExceptions) m a -> m a
force action = runEMT $ action `catchWithSrcLoc` 
                               \loc (e :: NoSuchRate) -> fail (showExceptionWithTrace loc e)

