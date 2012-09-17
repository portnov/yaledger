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

import YaLedger.Types
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

      r = all matches $ M.assocs qAttributes

      matches (name, avalue) =
        case M.lookup name getAttributes of
          Nothing  -> False
          Just val -> matchAV val avalue

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

convertDecimal :: Throws NoSuchRate l
               => Currency
               -> Posting Decimal t
               -> Ledger l Decimal
convertDecimal to (DPosting acc a) = do
  x :# _ <- convert to (a :# getCurrency acc)
  return x
convertDecimal to (CPosting acc a) = do
  x :# _ <- convert to (a :# getCurrency acc)
  return x

checkEntry :: (Throws NoSuchRate l,
               Throws NoCorrespondingAccountFound l,
               Throws InvalidAccountType l,
               Throws InternalError l)
             => Attributes
             -> Entry Amount Unchecked
             -> Ledger l (Entry Decimal Checked)
checkEntry attrs (UEntry dt cr mbCorr currs) = do
  defcur <- gets lsDefaultCurrency
  let currencies    = uniq $ map getCurrency cr ++ map getCurrency dt ++ [defcur]
      nCurrencies   = length $ nub $ sort $
                          map getCurrency cr ++
                          map getCurrency dt ++
                          map (getCurrency . creditPostingAccount) cr ++
                          map (getCurrency . debitPostingAccount)  dt
      firstCurrency = head currencies
      accounts      = map (getID . creditPostingAccount) cr
                   ++ map (getID . debitPostingAccount) dt
      accountNames  = map (getName . creditPostingAccount) cr
                   ++ map (getName . debitPostingAccount) dt
      source        = head accountNames

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

  e@(CEntry dtF crF _) <-
            -- Should we add credit / debit posting
            -- to entry?
            if dtSum == crSum
              then return $ CEntry dt' cr' OneCurrency
              else do
                   let diff = crSum - dtSum -- in firstCurrency
                   -- Fill credit / debit entry parts
                   fillEntry dt' cr'
                             accounts
                             mbCorr
                             source
                             diff
                             (currencies ++ currs)
                             attrs firstCurrency
  -- If there is more than 1 currency,
  -- then we should calculate rates difference.
  if traceS "currencies: " nCurrencies > 1
    then do
         -- Convert all postings into default currency
         crD <- mapM (convertDecimal defcur) crF
         dtD <- mapM (convertDecimal defcur) dtF

         let diffD :: Decimal -- In default currency
             diffD = sum crD - sum dtD
         rd <- if diffD == 0
                 then return OneCurrency
                 else do
                      correspondence <- lookupCorrespondingAccount (M.insert "category" (Exactly "rates-difference") attrs)
                                                                   source
                                                                   accounts
                                                                   diffD
                                                                   [defcur] -- Use only accounts in default currency
                                                                   Nothing
                      if diffD < 0
                        then do
                             account <- accountAsCredit correspondence
                             return $ CreditDifference $ CPosting account (-diffD)
                        else do
                             account <- accountAsDebit correspondence
                             return $ DebitDifference $ DPosting account diffD
         return $ e {cEntryRatesDifference = rd}
    else return e

lookupCorrespondingAccount :: (Throws NoCorrespondingAccountFound l)
                           => Attributes
                           -> String
                           -> [AccountID]
                           -> Decimal
                           -> [Currency]
                           -> Maybe AnyAccount
                           -> Ledger l AnyAccount
lookupCorrespondingAccount attrs source accounts value currencies mbCorr = do
  plan <- gets lsAccountPlan
  amap <- gets lsAccountMap
  let qry = CQuery {
               cqType = if value < 0
                          then ECredit
                          else EDebit,
               cqCurrency = currencies,
               cqExcept = accounts,
               cqAttributes = M.insert "source" (Exactly source) attrs
             }
  let mbAccount = runCQuery qry plan
      mbByMap = lookupAMap plan amap qry accounts
  case mbByMap `mplus` mbAccount of
    Nothing -> case mbCorr of
                 Nothing -> throw (NoCorrespondingAccountFound qry)
                 Just acc -> return acc
    Just acc -> return acc

fillEntry :: (Throws NoSuchRate l,
              Throws NoCorrespondingAccountFound l,
              Throws InvalidAccountType l,
              Throws InternalError l)
           => [Posting Decimal Debit]
           -> [Posting Decimal Credit]
           -> [AccountID]
           -> Maybe AnyAccount
           -> String
           -> Decimal
           -> [Currency]
           -> Attributes
           -> Currency
           -> Ledger l (Entry Decimal Checked)
fillEntry dt cr accounts mbCorr source value currencies attrs currency = do
  correspondence <- lookupCorrespondingAccount attrs source accounts value currencies mbCorr
  if value < 0
     then do
          account <- accountAsCredit correspondence
          -- Convert value into currency of found account
          value' :# _ <- convert (getCurrency account) (value :# currency)
          let e = CPosting account (-value')
          -- Will fill rates difference later
          return $ CEntry dt (e:cr) OneCurrency
     else do
          account <- accountAsDebit correspondence
          -- Convert value into currency of found account
          value' :# _ <- convert (getCurrency account) (value :# currency)
          let e = DPosting account value'
          -- Will fill rates difference later
          return $ CEntry (e:dt) cr OneCurrency

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
              qAttributes = M.empty }

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
         let posting = DPosting account' ((-diff) :# accountCurrency)
         return $ UEntry [posting] [] Nothing [getCurrency amount]

