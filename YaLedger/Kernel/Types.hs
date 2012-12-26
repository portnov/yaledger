{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}

module YaLedger.Kernel.Types where

import Control.Monad.Exception
import Data.Decimal

import YaLedger.Exceptions
import YaLedger.Types

-- | Accounts that could be credited
class CanCredit a where
  credit :: (Throws InternalError l,
             Throws InsufficientFunds l)
         => a
         -> Ext (Posting Decimal Credit)
         -> Atomic l ()

  creditHold :: (Throws InternalError l) 
         => a
         -> Ext (Hold Decimal Credit)
         -> Atomic l ()

-- | Accounts that could be debited
class CanDebit a where
  debit :: (Throws InternalError l,
            Throws InsufficientFunds l)
        => a
        -> Ext (Posting Decimal Debit)
        -> Atomic l ()

  debitHold :: (Throws InternalError l)
         => a
         -> Ext (Hold Decimal Debit)
         -> Atomic l ()

class Sign t => HoldOperations t where
  justHold :: t -> Decimal -> Balance Checked
  addHoldSum :: t -> Decimal -> Balance Checked -> Balance Checked
  getHolds :: FreeOr t Account -> History (Hold Decimal) t

instance HoldOperations Credit where
  justHold _ x = Balance Nothing 0 x 0
  addHoldSum _ x b = b {creditHolds = creditHolds b + x}

  getHolds (Left  a) = freeAccountCreditHolds a
  getHolds (Right a) = creditAccountHolds a

instance HoldOperations Debit where
  justHold _ x = Balance Nothing 0 0 x
  addHoldSum _ x b = b {debitHolds = debitHolds b - x}

  getHolds (Left  a) = freeAccountDebitHolds a
  getHolds (Right a) = debitAccountHolds a

