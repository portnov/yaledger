{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Kernel where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc
import Control.Monad.Trans
import Data.List
import Data.Dates
import Data.Decimal
import qualified Data.Map as M
import Text.Regex.PCRE

import YaLedger.Types
import YaLedger.Tree
import YaLedger.Monad
import YaLedger.Exceptions
import YaLedger.Correspondence

instance CanDebit Debit where
  debit acc@(DAccount {..}) e =
      acc {debitAccountEntries = e: debitAccountEntries}

instance CanDebit Free where
  debit acc@(FAccount {..}) e =
      acc {freeAccountDebitEntries = e: freeAccountDebitEntries}

instance CanCredit Credit where
  credit acc@(CAccount {..}) e =
      acc {creditAccountEntries = e: creditAccountEntries}

instance CanCredit Free where
  credit acc@(FAccount {..}) e =
      acc {freeAccountCreditEntries = e: freeAccountCreditEntries}

convert :: (Throws NoSuchRate l)
        => Currency -> Amount -> Ledger l Amount
convert c' (x :# c)
  | c == c' = return (x :# c)
  | otherwise = do
    rs <- gets lsRates
    case M.lookup (c, c') rs of
      Nothing   -> throw (NoSuchRate c c')
      Just rate -> return $ (x *. rate) :# c'

checkQuery :: Query -> Ext a -> Bool
checkQuery (Query {..}) (Ext {..}) =
  let p = case qStart of
            Just s  -> getDate >= s
            Nothing -> True

      q = case qEnd of
            Just e -> getDate <= e
            Nothing -> True

      r = all matches qAttributes

      matches (name, regex) =
        case lookup name getAttributes of
          Nothing  -> False
          Just val -> val =~ regex

  in  p && q && r

creditEntries :: AnyAccount -> [Ext (Entry Credit)]
creditEntries (WCredit _ (CAccount {..})) = creditAccountEntries
creditEntries (WDebit  _ (DAccount {..})) = []
creditEntries (WFree   _ (FAccount {..})) = freeAccountCreditEntries

debitEntries :: AnyAccount -> [Ext (Entry Debit)]
debitEntries (WCredit _ (CAccount {..})) = []
debitEntries (WDebit  _ (DAccount {..})) = debitAccountEntries
debitEntries (WFree   _ (FAccount {..})) = freeAccountDebitEntries

saldo :: (Throws NoSuchRate l)
      => Query -> AnyAccount -> Ledger l Amount
saldo query account = do
  rs <- gets lsRates
  let c = getCurrency account
  cr :# _ <- sumEntries c $ map getContent $ filter (checkQuery query) $ creditEntries account
  dt :# _ <- sumEntries c $ map getContent $ filter (checkQuery query) $ debitEntries  account
  return $ (cr - dt) :# c

sumEntries :: (Throws NoSuchRate l)
           => Currency -> [Entry t] -> Ledger l Amount
sumEntries c es = do
    rs <- gets lsRates
    ams <- mapM (convert c) (map getAmount es)
    let s = sum [x | x :# _ <- ams]
    return (s :# c)

accountAsCredit :: (Throws InvalidAccountType l)
                => AnyAccount
                -> Ledger l (FreeOr Credit Account)
accountAsCredit (WFree   _ a) = return $ Left a
accountAsCredit (WCredit _ a) = return $ Right a
accountAsCredit _ = throw $ InvalidAccountType AGDebit AGCredit

accountAsDebit :: (Throws InvalidAccountType l)
                => AnyAccount
                -> Ledger l (FreeOr Debit Account)
accountAsDebit (WFree   _ a) = return $ Left a
accountAsDebit (WDebit _ a) = return $ Right a
accountAsDebit _ = throw $ InvalidAccountType AGCredit AGDebit

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x:xs) = x: uniq (filter (/= x) xs)

checkPosting :: (Throws NoSuchRate l,
                 Throws NoCorrespondingAccountFound l,
                 Throws InvalidAccountType l)
             => Attributes
             -> Posting Unchecked
             -> Ledger l (Posting Checked)
checkPosting attrs (UPosting dt cr mbCorr) = do
  rs <- gets lsRates
  plan <- gets lsAccountPlan
  defcur <- gets lsDefaultCurrency
  let currencies = uniq $ map getCurrency cr ++ map getCurrency dt ++ [defcur]
      firstCurrency = head currencies
  dtSum :# _ <- sumEntries firstCurrency dt
  crSum :# _ <- sumEntries firstCurrency cr
  if dtSum == crSum
    then return $ CPosting dt cr
    else do
         let diff = crSum - dtSum
             qry = CQuery {
                     cqType = if diff > 0
                                then ECredit
                                else EDebit,
                     cqCurrency = currencies,
                     cqAttributes = attrs }
         let mbAccount = runCQuery qry plan
         case mbCorr `mplus` mbAccount of
           Nothing -> throw (NoCorrespondingAccountFound qry)
           Just acc -> if diff > 0
                         then do
                              account <- accountAsCredit acc
                              let e = CEntry account (diff :# firstCurrency)
                              return $ CPosting dt (e:cr)
                         else do
                              account <- accountAsDebit acc
                              let e = DEntry account (diff :# firstCurrency)
                              return $ CPosting (e:dt) cr

updateAccount :: Integer
              -> AccountPlan
              -> (AnyAccount -> Ledger l AnyAccount)
              -> Ledger l AccountPlan
updateAccount i leaf@(Leaf _ _ acc) fn
  | getID acc == i = do
                     acc' <- fn acc
                     return $ leaf {leafData = acc'}
  | otherwise      = return leaf
updateAccount i branch@(Branch _ _ ag children) fn
  | i `inRange` agRange ag = do
    children' <- mapM (\c -> updateAccount i c fn) children
    return $ branch {branchChildren = children'}
  | otherwise = return branch

updatePlan :: (AccountPlan -> Ledger l AccountPlan) -> Ledger l ()
updatePlan fn = do
  st <- get
  let plan = lsAccountPlan st
  plan' <- fn plan
  put $ st {lsAccountPlan = plan'}

