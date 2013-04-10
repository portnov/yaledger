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
import YaLedger.Kernel.Query
import YaLedger.Logger
import YaLedger.Output.Pretty

isHoldOpen :: Maybe DateTime       -- ^ If Just date, check if hold was created after this date
           -> Maybe DateTime       -- ^ If Just date, check if hold was closed after this date
           -> Ext (Hold Decimal t) -- ^ Hold itself
           -> Bool
isHoldOpen mbStart mbEnd extHold =
  let startOk = case mbStart of
                  Nothing -> True
                  Just startDate -> getDate extHold >= startDate
      endOk   = case mbEnd of
                  Nothing -> True
                  Just endDate -> case holdEndDate (getContent extHold) of
                                    Nothing   -> True
                                    Just date -> date > endDate
  in startOk && endOk

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
           -> Maybe (Entry Decimal Checked)
           -> (Decimal -> Decimal -> Bool)  -- ^ Operation to check hold amount, such as (==) or (>=) 
           -> Attributes                    -- ^ Hold attributes
           -> Posting Decimal t             -- ^ Hold posting
           -> Atomic l ()
closeHold date mbEntry op qry posting = do
    infoSTM $ "Closing hold: " ++ prettyPrint posting
    let account = postingAccount' posting
        history = getHolds account
    holds <- readIOList history
    anyClosed <- close [] history holds
    if anyClosed
      then return ()
      else throwP =<< noSuchHold posting
  where
    searchAmt = traceS "searchAmt" $ postingValue posting

    -- Close any appropriate hold

    -- No appropriate holds were found in histroy.
    close [] _ [] = return False
    -- 
    close acc history [] = do
        -- updateBalances searchAmt (postingAccount posting)
        stm $ writeTVar history acc
        return False
    close acc history (extHold: rest) = do
      debugSTM $ "Checking hold: " ++ show extHold
      if checkHold op date searchAmt qry extHold
        then do
             let oldHold = getContent extHold
                 holdAmt = traceS "holdAmt" $ postingValue $ holdPosting oldHold
                 closedHold = extHold {getContent = oldHold {holdEndDate = Just date}}
             debugSTM $ "Creating new hold: holdAmt = " ++ show holdAmt ++ ", searchAmt = " ++ show searchAmt
             newHolds <- if holdAmt > searchAmt
                           then do
                                -- Found hold amount > requested posting amount.
                                -- We must add new hold for difference.
                                let account = postingAccount' $ holdPosting oldHold
                                    newPosting = createPosting account (holdAmt - searchAmt)
                                debugSTM $ "New hold posting: " ++ show newPosting
                                return [extHold {getDate = date,
                                                getContent = Hold newPosting Nothing}]
                           else -- holdAmt <= searchAmt, we'll just close old hold.
                                return []
             let holdChange = if holdAmt > searchAmt
                                then searchAmt
                                else holdAmt
             updateBalances holdChange (postingAccount posting)
                 
             stm $ writeTVar history (acc ++ newHolds ++ [closedHold] ++ rest)
             return True
        else close (acc ++ [extHold]) history rest

    -- Update balances by amount of posting:
    -- * for credit posting, decrease creditHolds amount
    -- * for debit  posting, increase debitHolds  amount
    updateBalances :: Decimal -> AnyAccount -> Atomic l ()
    updateBalances amt account = do
      debugSTM $ "updateBalances: " ++ getName account ++ " by " ++ show amt
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

-- | Get all holds of given account
getHoldsHistory :: (Throws InternalError l)
                => Query
                -> AnyAccount
                -> Ledger l [Either (Ext (Hold Decimal Credit)) (Ext (Hold Decimal Debit))]
getHoldsHistory qry account = do
  e1 <- wrapIO $ newTVarIO []
  e2 <- wrapIO $ newTVarIO []
  let creditHoldsHistory = case account of
                             WFree a -> freeAccountCreditHolds a
                             WCredit a -> creditAccountHolds a
                             WDebit a -> e1
      debitHoldsHistory  = case account of
                             WFree a -> freeAccountDebitHolds a
                             WCredit a -> e2
                             WDebit a -> debitAccountHolds a
  creditHolds <- runAtomically $ readIOList creditHoldsHistory
  debitHolds  <- runAtomically $ readIOList debitHoldsHistory
  return $ map Left  (filter (checkQuery qry) creditHolds) ++
           map Right (filter (checkQuery qry) debitHolds)


-- | Smart constructor for NoSuchHold
noSuchHold :: (Throws NoSuchHold l) => Posting Decimal t -> Atomic l (SourcePos -> NoSuchHold)
noSuchHold (CPosting acc amt _) = do
  coa <- gets lsCoA
  let Just path = accountFullPath (getID acc) coa
  return (NoSuchHold ECredit amt path)
noSuchHold (DPosting acc amt _) = do
  coa <- gets lsCoA
  let Just path = accountFullPath (getID acc) coa
  return (NoSuchHold EDebit amt path)

