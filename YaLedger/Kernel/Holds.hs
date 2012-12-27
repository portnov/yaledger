{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}

module YaLedger.Kernel.Holds where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Concurrent.STM
import Data.Decimal
import Data.Dates
import qualified Data.Map as M

import YaLedger.Types
import YaLedger.Exceptions
import YaLedger.Kernel.Types
import YaLedger.Kernel.Common
import YaLedger.Logger
import YaLedger.Output.Pretty

-- | Check if hold is suitable to close or use
checkHold :: (Decimal -> Decimal -> Bool) -- ^ Operation to check hold amount, (==) or (>=)
          -> DateTime                     -- ^ Transaction date/time
          -> Decimal                      -- ^ Transaction amount
          -> Ext (Hold Decimal t)         -- ^ Hold to check
          -> Bool
checkHold op date amt extHold =
    (getDate extHold <= date) &&
    (case holdEndDate (getContent extHold) of
       Nothing -> True
       Just dt -> dt >= date ) &&
    ((postingValue $ holdPosting $ getContent extHold) `op` amt)

-- | Close one hold.
closeHold :: forall t l.
             (HoldOperations t,
              Throws NoSuchHold l,
              Throws InternalError l)
           => DateTime               -- ^ Transaction date/time
           -> Posting Decimal t      -- ^ Hold posting
           -> Atomic l ()
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

    close [] _ _ [] = return False
    close acc _ history [] = do
        updateBalances (postingAccount posting)
        stm $ writeTVar history acc
        return False
    close acc op history (extHold: rest) =
      if checkHold op date amt extHold
        then do
             let oldHold = getContent extHold
                 newHold = extHold {getContent = oldHold {holdEndDate = Just date}}
             updateBalances (postingAccount posting)
             stm $ writeTVar history (acc ++ [newHold] ++ rest)
             return True
        else close (acc ++ [extHold]) op history rest

    updateBalances :: AnyAccount -> Atomic l ()
    updateBalances account =
      plusIOList zeroExtBalance (const True) updateExtBalance (accountBalances account)

    zeroExtBalance =
      Ext {
        getDate = date,
        extID = 0,
        getLocation = nowhere,
        getAttributes = M.empty,
        getContent = zeroBalance }

    updateExtBalance :: Ext (Balance Checked) -> Ext (Balance Checked)
    updateExtBalance extBalance =
      Ext {
        getDate = date,
        extID = 0,
        getLocation = nowhere,
        getAttributes = M.empty,
        getContent = addHoldSum (undefined :: t) (negate amt) (getContent extBalance)
      }

-- | Smart constructor for NoSuchHold
noSuchHold :: (Throws NoSuchHold l) => Posting Decimal t -> Atomic l b
noSuchHold (CPosting acc amt _) = do
  coa <- gets lsCoA
  let Just path = accountFullPath (getID acc) coa
  throwP (NoSuchHold ECredit amt path)
noSuchHold (DPosting acc amt _) = do
  coa <- gets lsCoA
  let Just path = accountFullPath (getID acc) coa
  throwP (NoSuchHold EDebit amt path)


