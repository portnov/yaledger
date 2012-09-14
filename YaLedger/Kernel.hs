{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Kernel where

import Prelude hiding (catch)
import Control.Applicative ((<$>))
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
import Text.Printf

import YaLedger.Types
import YaLedger.Tree
import YaLedger.Monad
import YaLedger.Exceptions
import YaLedger.Correspondence

class CanCredit a where
  credit :: (Throws InternalError l)
         => a
         -> Ext (Posting Decimal Credit)
         -> Ledger l ()

class CanDebit a where
  debit :: (Throws InternalError l)
        => a
        -> Ext (Posting Decimal Debit)
        -> Ledger l ()

instance CanDebit (Account Debit) where
  debit (DAccount {..}) p = do
      appendIOList debitAccountPostings p

instance CanDebit (Account Free) where
  debit (FAccount {..}) p = do
      appendIOList freeAccountDebitPostings p

instance CanCredit (Account Credit) where
  credit (CAccount {..}) p = do
      appendIOList creditAccountPostings p

instance CanCredit (Account Free) where
  credit (FAccount {..}) p = do
      appendIOList freeAccountCreditPostings p

instance CanCredit (FreeOr Credit Account) where
  credit (Left  a) p = credit a p
  credit (Right a) p = credit a p

instance CanDebit (FreeOr Debit Account) where
  debit (Left  a) p = debit a p
  debit (Right a) p = debit a p

convert :: (Throws NoSuchRate l)
        => Currency -> Amount -> Ledger l Amount
convert c' (x :# c)
  | c == c' = return (x :# c)
  | otherwise = do
    rs <- gets lsRates
    case M.lookup (c, c') rs of
      Nothing   -> throw (NoSuchRate c c')
      Just rate -> return $ (x *. rate) :# c'

convert' :: Currency -> Amount -> Ledger l Amount
convert' c x = 
  convert c x
    `catch`
      \(_ :: NoSuchRate) -> return x

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

creditPostings :: Throws InternalError l
               => AnyAccount
               -> Ledger l (AccountHistory Credit)
creditPostings (WCredit _ (CAccount {..})) = return creditAccountPostings
creditPostings (WDebit  _ (DAccount {..})) = newIOList
creditPostings (WFree   _ (FAccount {..})) = return freeAccountCreditPostings

debitPostings :: Throws InternalError l
              => AnyAccount
              -> Ledger l (AccountHistory Debit)
debitPostings (WCredit _ (CAccount {..})) = newIOList
debitPostings (WDebit  _ (DAccount {..})) = return debitAccountPostings
debitPostings (WFree   _ (FAccount {..})) = return freeAccountDebitPostings

saldo :: (Throws NoSuchRate l,
          Throws InternalError l)
      => Query -> AnyAccount -> Ledger l Amount
saldo query account = do
  rs <- gets lsRates

  let c = getCurrency account

      filterPostings :: [Ext (Posting Decimal t)] -> [Posting Decimal t]
      filterPostings list = map getContent $ filter (checkQuery query) list

  crp <- filterPostings <$> (readIOList =<< creditPostings account)
  dtp <- filterPostings <$> (readIOList =<< debitPostings  account)
  let cr = sumPostings crp
      dt = sumPostings dtp
  return $ (cr - dt) :# c

sumPostings :: [Posting Decimal t] -> Decimal
sumPostings es = sum (map go es)
  where
    go (CPosting _ x) = x
    go (DPosting _ x) = x

accountByID :: AccountID -> AccountPlan -> Maybe AnyAccount
accountByID i (Branch _ _ ag children)
  | i `inRange` agRange ag = do
      let accs = [acc | Leaf _ _ acc <- children]
      case filter (\a -> getID a == i) accs of
        [x] -> return x
        _   -> do
               let grps = [grp | Branch _ _ _ grp <- children]
               first (accountByID i) (concat grps)
  | otherwise = Nothing

accountByID i (Leaf _ _ acc)
  | getID acc == i = Just acc
  | otherwise      = Nothing

accountAsCredit :: (Throws InvalidAccountType l)
                => AnyAccount
                -> Ledger l (FreeOr Credit Account)
accountAsCredit (WDebit  _ _) = throw $ InvalidAccountType AGDebit AGCredit
accountAsCredit (WCredit _ a) = return $ Right a
accountAsCredit (WFree   _ a) = return $ Left a

accountAsDebit :: (Throws InvalidAccountType l)
                => AnyAccount
                -> Ledger l (FreeOr Debit Account)
accountAsDebit (WCredit _ _) = throw $ InvalidAccountType AGCredit AGDebit
accountAsDebit (WDebit  _ a) = return $ Right a
accountAsDebit (WFree   _ a) = return $ Left a

accountByIDM :: AccountID -> Ledger l AnyAccount
accountByIDM aid = do
  plan <- gets lsAccountPlan
  case accountByID aid plan of
   Nothing -> fail $ "Internal error: no such account: " ++ show aid
   Just acc -> return acc

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x:xs) = x: uniq (filter (/= x) xs)

convertPosting :: Throws NoSuchRate l
               => Currency
               -> Posting Amount t
               -> Ledger l (Posting Decimal t)
convertPosting to (DPosting acc a) = do
  x :# _ <- convert to a
  return $ DPosting acc x
convertPosting to (CPosting acc a) = do
  x :# _ <- convert to a
  return $ CPosting acc x

convertPosting' :: Throws NoSuchRate l
               => Posting Amount t
               -> Ledger l (Posting Decimal t)
convertPosting' (DPosting acc a) = do
  x :# _ <- convert (getCurrency acc) a
  return $ DPosting acc x
convertPosting' (CPosting acc a) = do
  x :# _ <- convert (getCurrency acc) a
  return $ CPosting acc x

checkEntry :: (Throws NoSuchRate l,
               Throws NoCorrespondingAccountFound l,
               Throws InvalidAccountType l,
               Throws InternalError l)
             => Attributes
             -> Entry Amount Unchecked
             -> Ledger l (Entry Decimal Checked)
checkEntry attrs src@(UEntry dt cr mbCorr currs) = do
  rs <- gets lsRates
  plan <- gets lsAccountPlan
  amap <- gets lsAccountMap
  defcur <- gets lsDefaultCurrency
  let currencies    = uniq $ map getCurrency cr ++ map getCurrency dt ++ [defcur]
      firstCurrency = head currencies
      accounts      = map (getID . creditPostingAccount) cr
                   ++ map (getID . debitPostingAccount) dt
      accountNames  = map (getName . creditPostingAccount) cr
                   ++ map (getName . debitPostingAccount) dt

  -- Convert all postings into firstCurrency
  dt1 <- mapM (convertPosting firstCurrency) dt
  cr1 <- mapM (convertPosting firstCurrency) cr

  -- And sum them to check if debit == credit
  let dtSum = sumPostings dt1 
      crSum = sumPostings cr1

  -- Convert each posting's sum into currency
  -- of posting's account
  dt' <- mapM convertPosting' dt
  cr' <- mapM convertPosting' cr

  if dtSum == crSum
    then return $ CEntry dt' cr'
    else do
         let diff = crSum - dtSum -- in firstCurrency
             qry = CQuery {
                     cqType = if diff < 0
                                then ECredit
                                else EDebit,
                     cqCurrency = currencies ++ currs,
                     cqExcept = accounts,
                     cqAttributes = attrs ++ [("source", head accountNames)]
                   }
         let mbAccount = runCQuery qry plan
             mbByMap = lookupAMap plan amap qry accounts
         case mbCorr `mplus` mbByMap `mplus` mbAccount of
           Nothing -> throw (NoCorrespondingAccountFound qry)
           Just acc -> if diff < 0
                         then do
                              account <- accountAsCredit acc
                              -- Convert diff into currency of found account
                              diff' :# _ <- convert (getCurrency account) (diff :# firstCurrency)
                              let e = CPosting account (-diff')
                              return $ CEntry dt' (e:cr')
                         else do
                              account <- accountAsDebit acc
                              -- Convert diff into currency of found account
                              diff' :# _ <- convert (getCurrency account) (diff :# firstCurrency)
                              let e = DPosting account diff'
                              return $ CEntry (e:dt') cr'

reconciliate :: (Throws NoSuchRate l,
                 Throws InvalidAccountType l,
                 Throws InternalError l)
             => DateTime
             -> AnyAccount
             -> Amount
             -> Ledger l (Entry Amount Unchecked)
reconciliate date account amount = do

  let qry = Query {
              qStart = Nothing,
              qEnd   = Just date,
              qAttributes = [] }

  currentBalance :# accountCurrency <- saldo qry account
  actualBalance :# _ <- convert (getCurrency account) amount

  -- diff is in accountCurrency
  let diff = actualBalance - currentBalance
  if diff > 0
    then do
         account' <- accountAsCredit account
         let posting = CPosting account' (diff :# accountCurrency)
         return $ UEntry [] [posting] Nothing [getCurrency amount]
    else do
         account' <- accountAsDebit account
         let posting = DPosting account' (-diff :# accountCurrency)
         return $ UEntry [posting] [] Nothing [getCurrency amount]

