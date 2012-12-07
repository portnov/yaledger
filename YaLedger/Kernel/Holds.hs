{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}

module YaLedger.Kernel.Holds where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Data.Decimal
import Data.Dates

import YaLedger.Types
import YaLedger.Exceptions
import YaLedger.Kernel
import YaLedger.Logger
import YaLedger.Output.Pretty

closeHold :: (HoldOperations t,
              Throws NoSuchHold l,
              Throws InternalError l)
           => DateTime
           -> Posting Decimal t
           -> Ledger l ()
closeHold date posting = do
    let acc = postingAccount' posting
        history = getHolds acc
    holds <- readIOList history
    anyClosed <- close [] (==) history holds
    if anyClosed
      then return ()
      else noSuchHold posting
  where
    amt = postingValue posting

    good op extHold =
      (getDate extHold <= date) &&
      ((postingValue $ holdPosting $ getContent extHold) `op` amt)

    close [] _ _ [] = return False
    close acc _ history [] = do
        writeIOList history acc
        return False
    close acc op history (extHold: rest) =
      if good op extHold
        then do
             let oldHold = getContent extHold
                 newHold = extHold {getContent = oldHold {holdEndDate = Just date}}
             writeIOList history (acc ++ [newHold] ++ rest)
             return True
        else close (acc ++ [extHold]) op history rest

noSuchHold :: (Throws NoSuchHold l) => Posting Decimal t -> Ledger l b
noSuchHold (CPosting acc amt) = do
  coa <- gets lsCoA
  let Just path = accountFullPath (getID acc) coa
  throwP (NoSuchHold ECredit amt path)
noSuchHold (DPosting acc amt) = do
  coa <- gets lsCoA
  let Just path = accountFullPath (getID acc) coa
  throwP (NoSuchHold EDebit amt path)


