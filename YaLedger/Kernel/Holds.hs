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
import YaLedger.Kernel.Balances
import YaLedger.Kernel.Correspondence
import YaLedger.Logger
import YaLedger.Output.Pretty

-- | Check if hold is suitable to close or use
checkHold :: (Decimal -> Decimal -> Bool) -- ^ Operation to check hold amount, (==) or (>=)
          -> DateTime                     -- ^ Transaction date/time
          -> Decimal                      -- ^ Transaction amount
          -> Attributes                   -- ^ Hold attributes
          -> Ext (Hold Decimal t)         -- ^ Hold to check
          -> Bool
checkHold op date amt qry extHold =
    (getDate extHold <= date) &&
    (case holdEndDate (getContent extHold) of
       Nothing -> True
       Just dt -> dt >= date ) &&
    ((postingValue $ holdPosting $ getContent extHold) `op` amt) &&
    (getAttributes extHold `matchAll` qry)

-- | Close one hold. If found hold's amount is greater than requested,
-- then close found hold and create a new one, with amount of
-- (found hold amount - requested amount).
closeHold :: forall t l.
             (HoldOperations t,
              Throws NoSuchHold l,
              Throws InternalError l)
           => DateTime                      -- ^ Transaction date/time
           -> (Decimal -> Decimal -> Bool)  -- ^ Operation to check hold amount, such as (==) or (>=) 
           -> Attributes                    -- ^ Hold attributes
           -> Posting Decimal t             -- ^ Hold posting
           -> Atomic l ()
closeHold date op qry posting = do
    infoSTM $ "Closing hold: " ++ prettyPrint posting
    let account = postingAccount' posting
        history = getHolds account
    holds <- readIOList history
    anyClosed <- close [] history holds
    if anyClosed
      then return ()
      else noSuchHold posting
  where
    searchAmt = postingValue posting

    -- Close any appropriate hold

    -- No appropriate holds were found in histroy.
    close [] _ [] = return False
    -- 
    close acc history [] = do
        updateBalances searchAmt (postingAccount posting)
        stm $ writeTVar history acc
        return False
    close acc history (extHold: rest) =
      trace ("Checking hold: " ++ show extHold) $
      if checkHold op date searchAmt qry extHold
        then do
             let oldHold = getContent extHold
                 holdAmt = postingValue $ holdPosting oldHold
                 closedHold = extHold {getContent = oldHold {holdEndDate = Just date}}
                 newHolds = if holdAmt > searchAmt
                             then -- Found hold amount > requested posting amount.
                                  -- We must add new hold for difference.
                                  let account = postingAccount' $ holdPosting oldHold
                                      newPosting = createPosting account (holdAmt - searchAmt)
                                  in  [extHold {getDate = date,
                                                getContent = Hold newPosting Nothing}]
                             else -- holdAmt <= searchAmt, we'll just close old hold.
                                  []
             updateBalances holdAmt (postingAccount posting)
             stm $ writeTVar history (acc ++ newHolds ++ [closedHold] ++ rest)
             return True
        else close (acc ++ [extHold]) history rest

    -- Update balances by amount of posting:
    -- * for credit posting, decrease creditHolds amount
    -- * for debit  posting, increase debitHolds  amount
    updateBalances :: Decimal -> AnyAccount -> Atomic l ()
    updateBalances amt account =
      plusIOList zeroExtBalance (const True) (updateExtBalance amt) (accountBalances account)

    zeroExtBalance =
      Ext {
        getDate = date,
        extID = 0,
        getLocation = nowhere,
        getAttributes = M.empty,
        getContent = zeroBalance }

    updateExtBalance :: Decimal -> Ext (Balance Checked) -> Ext (Balance Checked)
    updateExtBalance amt extBalance =
      Ext {
        getDate = date,
        extID = 0,
        getLocation = nowhere,
        getAttributes = M.empty,
        getContent = (addHoldSum (undefined :: t) (negate amt) (getContent extBalance)) {causedBy = Nothing}
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

