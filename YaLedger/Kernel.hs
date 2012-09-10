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
      acc {debitAccountPostings = e: debitAccountPostings}

instance CanDebit Free where
  debit acc@(FAccount {..}) e =
      acc {freeAccountDebitPostings = e: freeAccountDebitPostings}

instance CanCredit Credit where
  credit acc@(CAccount {..}) e =
      acc {creditAccountPostings = e: creditAccountPostings}

instance CanCredit Free where
  credit acc@(FAccount {..}) e =
      acc {freeAccountCreditPostings = e: freeAccountCreditPostings}

convert :: (AmountKind v, Throws NoSuchRate l)
        => Currency -> Amount v -> Ledger l (Amount v)
convert c' (x :# c)
  | c == c' = return (x :# c)
  | otherwise = do
    rs <- gets lsRates
    case M.lookup (c, c') rs of
      Nothing   -> throw (NoSuchRate c c')
      Just rate -> return $ (x `multiply` rate) :# c'

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

creditPostings :: AnyAccount -> [Ext (Posting Decimal Credit)]
creditPostings (WCredit _ (CAccount {..})) = creditAccountPostings
creditPostings (WDebit  _ (DAccount {..})) = []
creditPostings (WFree   _ (FAccount {..})) = freeAccountCreditPostings

debitPostings :: AnyAccount -> [Ext (Posting Decimal Debit)]
debitPostings (WCredit _ (CAccount {..})) = []
debitPostings (WDebit  _ (DAccount {..})) = debitAccountPostings
debitPostings (WFree   _ (FAccount {..})) = freeAccountDebitPostings

saldo :: (Throws NoSuchRate l)
      => Query -> AnyAccount -> Ledger l (Amount Decimal)
saldo query account = do
  rs <- gets lsRates
  let c = getCurrency account
  cr :# _ <- sumPostings c $ map getContent $ filter (checkQuery query) $ creditPostings account
  dt :# _ <- sumPostings c $ map getContent $ filter (checkQuery query) $ debitPostings  account
  return $ (cr `minus` dt) :# c

sumV :: AmountKind v => [v] -> v
sumV xs = foldr plus zero xs

sumPostings :: (AmountKind v, Throws NoSuchRate l)
           => Currency -> [Posting v t] -> Ledger l (Amount v)
sumPostings c es = do
    rs <- gets lsRates
    ams <- mapM (convert c) (map getAmount es)
    let s = sumV [x | x :# _ <- ams]
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

checkEntry :: (Throws NoSuchRate l,
                 Throws NoCorrespondingAccountFound l,
                 Throws InvalidAccountType l)
             => Attributes
             -> Entry Decimal Unchecked
             -> Ledger l (Entry Decimal Checked)
checkEntry attrs src@(UEntry dt cr mbCorr) = do
  rs <- gets lsRates
  plan <- gets lsAccountPlan
  amap <- gets lsAccountMap
  defcur <- gets lsDefaultCurrency
  let currencies = uniq $ map getCurrency cr ++ map getCurrency dt ++ [defcur]
      firstCurrency = head currencies
      accounts = map (getID . creditPostingAccount) cr ++ map (getID . debitPostingAccount) dt
  dtSum :# _ <- sumPostings firstCurrency dt
  crSum :# _ <- sumPostings firstCurrency cr
  if dtSum == crSum
    then return $ CEntry dt cr
    else do
         let diff = crSum - dtSum
             qry = CQuery {
                     cqType = if diff > 0
                                then ECredit
                                else EDebit,
                     cqCurrency = currencies,
                     cqAttributes = attrs }
         let mbAccount = runCQuery qry plan
             mbByMap = lookupAMap plan amap qry accounts
         case mbCorr `mplus` mbByMap `mplus` mbAccount of
           Nothing -> throw (NoCorrespondingAccountFound qry)
           Just acc -> if diff > 0
                         then do
                              account <- accountAsCredit acc
                              message $ "Corresponding account for " ++ show src ++ ": " ++ show account
                              let e = CPosting account (diff :# firstCurrency)
                              return $ CEntry dt (e:cr)
                         else do
                              account <- accountAsDebit acc
                              message $ "Corresponding account for " ++ show src ++ ": " ++ show account
                              let e = DPosting account (diff :# firstCurrency)
                              return $ CEntry (e:dt) cr

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

