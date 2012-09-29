{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Kernel
  (module YaLedger.Kernel.Common,
   CanCredit (..), CanDebit (..),
   negateAmount, differenceType,
   convert, convert', convertPosting,
   convertPosting', convertDecimal,
   checkQuery,
   creditPostings, debitPostings,
   accountByID,
   sumGroup, sumPostings,
   accountAsCredit,
   accountAsDebit,
   checkEntry,
   reconciliate,
   saldo, treeSaldo
  ) where

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
import YaLedger.Kernel.Common
import YaLedger.Logger

class CanCredit a where
  credit :: (Throws InternalError l,
             Throws InsufficientFunds l)
         => a
         -> Ext (Posting Decimal Credit)
         -> Ledger l ()

class CanDebit a where
  debit :: (Throws InternalError l,
            Throws InsufficientFunds l)
        => a
        -> Ext (Posting Decimal Debit)
        -> Ledger l ()

instance CanDebit (Account Debit) where
  debit acc@(DAccount {..}) p = do
      balance <- getCurrentBalance acc
      checkBalance (balance - postingValue (getContent p)) acc
      appendIOList debitAccountPostings p
      balancePlus p debitAccountBalances

instance CanDebit (Account Free) where
  debit acc@(FAccount {..}) p = do
      balance <- getCurrentBalance acc
      checkBalance (balance - postingValue (getContent p)) acc
      appendIOList freeAccountDebitPostings p
      balancePlus p freeAccountBalances

instance CanCredit (Account Credit) where
  credit acc@(CAccount {..}) p = do
      balance <- getCurrentBalance acc
      checkBalance (balance + postingValue (getContent p)) acc
      appendIOList creditAccountPostings p
      balancePlus p creditAccountBalances

instance CanCredit (Account Free) where
  credit acc@(FAccount {..}) p = do
      balance <- getCurrentBalance acc
      checkBalance (balance + postingValue (getContent p)) acc
      appendIOList freeAccountCreditPostings p
      balancePlus p freeAccountBalances

instance CanCredit (FreeOr Credit Account) where
  credit (Left  a) p = credit a p
  credit (Right a) p = credit a p

instance CanDebit (FreeOr Debit Account) where
  debit (Left  a) p = debit a p
  debit (Right a) p = debit a p

balancePlus :: forall l t.
               (Throws InternalError l, Sign t)
            => Ext (Posting Decimal t)
            -> History Balance Checked
            -> Ledger l ()
balancePlus p history = do
  let s = fromIntegral (sign (undefined :: t))
      value = s * postingValue (getContent p)
      update e@(Ext {getContent = b}) =
          Ext {
            getDate       = getDate p,
            getLocation   = getLocation p,
            getAttributes = getAttributes p,
            getContent    = b {balanceValue = balanceValue b + value}
          }
  let zero = Ext (getDate p) (getLocation p) (getAttributes p) (Balance Nothing value)
  plusIOList zero update history

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing  _  = return ()
whenJust (Just a) fn = fn a

getCurrentBalance :: (IsAccount a,
                      Throws InternalError l)
                  => a
                  -> Ledger l Decimal
getCurrentBalance acc = do
  balances <- readIOList (accountBalances acc)
  return $ case balances of
             [] -> 0
             (b:_) -> balanceValue (getContent b)

checkBalance :: (Throws InternalError l,
                 Throws InsufficientFunds l,
                 IsAccount a)
             => Decimal
             -> a
             -> Ledger l ()
checkBalance targetBalance acc = do
  let bc = accountChecks acc
      op = if sign acc > 0
             then (>=)
             else (<=)
  whenJust (bcInfo bc) $ \value ->
    when (targetBalance `op` value) $
      info $ "Balance of " ++ getName acc ++ " will be " ++ show targetBalance ++ getCurrency acc
  whenJust (bcWarning bc) $ \value ->
    when (targetBalance `op` value) $ 
      warning $ "Balance of " ++ getName acc ++ " will be " ++ show targetBalance ++ getCurrency acc
  whenJust (bcError bc) $ \value ->
    when (targetBalance `op` value) $ 
      throwP (InsufficientFunds (getName acc) targetBalance (getCurrency acc))

negateAmount :: Amount -> Amount
negateAmount (x :# c) = (-x) :# c

differenceType :: Decimal -> PostingType
differenceType x
  | x < 0     = ECredit
  | otherwise = EDebit

convert :: (Throws NoSuchRate l)
        => Currency -> Amount -> Ledger l Amount
convert c' (x :# c)
  | c == c' = return (x :# c)
  | otherwise = do
    rs <- gets lsRates
    case M.lookup (c, c') rs of
      Nothing   -> throwP (NoSuchRate c c')
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
               -> Ledger l (History (Posting Decimal) Credit)
creditPostings (WCredit _ (CAccount {..})) = return creditAccountPostings
creditPostings (WDebit  _ (DAccount {..})) = newIOList
creditPostings (WFree   _ (FAccount {..})) = return freeAccountCreditPostings

debitPostings :: Throws InternalError l
              => AnyAccount
              -> Ledger l (History (Posting Decimal) Debit)
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
sumPostings es = sum (map postingValue es)

accountByID :: AccountID -> ChartOfAccounts -> Maybe AnyAccount
accountByID i (Branch _ ag children)
  | i `inRange` agRange ag = do
      let accs = [acc | Leaf _ acc <- children]
      case filter (\a -> getID a == i) accs of
        [x] -> return x
        _   -> do
               let grps = [grp | Branch _ _ grp <- children]
               first (accountByID i) (concat grps)
  | otherwise = Nothing

accountByID i (Leaf _ acc)
  | getID acc == i = Just acc
  | otherwise      = Nothing

accountAsCredit :: (Throws InvalidAccountType l)
                => AnyAccount
                -> Ledger l (FreeOr Credit Account)
accountAsCredit (WDebit  _ a) = throwP $ InvalidAccountType (getName a) AGDebit AGCredit
accountAsCredit (WCredit _ a) = return $ Right a
accountAsCredit (WFree   _ a) = return $ Left a

accountAsDebit :: (Throws InvalidAccountType l)
                => AnyAccount
                -> Ledger l (FreeOr Debit Account)
accountAsDebit (WCredit _ a) = throwP $ InvalidAccountType (getName a) AGCredit AGDebit
accountAsDebit (WDebit  _ a) = return $ Right a
accountAsDebit (WFree   _ a) = return $ Left a

accountByIDM :: AccountID -> Ledger l AnyAccount
accountByIDM aid = do
  coa <- gets lsCoA
  case accountByID aid coa of
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

-- | Check an entry:
--
--  * Enshure that credit == debit; if no, add needed postings.
--
--  * Convert amounts in each posting to it's account currency.
--
--  * Calculate exchange rate difference.
--
checkEntry :: (Throws NoSuchRate l,
               Throws NoCorrespondingAccountFound l,
               Throws InvalidAccountType l,
               Throws InternalError l)
             => Attributes               -- ^ Transaction attributes
             -> Entry Amount Unchecked   -- ^ Unchecked entry
             -> Ledger l (Entry Decimal Checked)
checkEntry attrs (UEntry dt cr mbCorr currs) = do
  defcur <- gets lsDefaultCurrency
  let currencies    = uniq $ map getCurrency cr ++ map getCurrency dt ++ [defcur]
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

  -- Should we add credit / debit posting
  -- to entry?
  (dtF, crF) <- if dtSum == crSum
                  then return (dt', cr')
                  else do
                       let diff = crSum - dtSum -- in firstCurrency
                       let qry = CQuery {
                                   cqType       = differenceType diff,
                                   cqCurrencies = currencies ++ currs,
                                   cqExcept     = accounts,
                                   cqAttributes = M.insert "source" (Optional source) attrs }
                       -- Fill credit / debit entry parts
                       fillEntry qry dt' cr' mbCorr (diff :# firstCurrency)
  let nCurrencies = length $ nub $ sort $
                          map getCurrency cr ++
                          map getCurrency dt ++
                          map (getCurrency . creditPostingAccount) crF ++
                          map (getCurrency . debitPostingAccount)  dtF
  -- If there is more than 1 currency,
  -- then we should calculate rates difference.
  if nCurrencies > 1
    then do
         debug $ "Credit: " ++ show (getAmount $ head crF) ++
                   ", Debit: " ++ show (getAmount $ head crF)
         -- Convert all postings into default currency
         crD <- mapM (convertDecimal defcur) crF
         dtD <- mapM (convertDecimal defcur) dtF
         debug $ "crD: " ++ show crD ++ ", dtD: " ++ show dtD

         let diffD :: Decimal -- In default currency
             diffD = sum crD - sum dtD
         rd <- if diffD == realFracToDecimal 10 (fromIntegral 0)
                 then return OneCurrency
                 else do
                      let attrs' = M.insert "category" (Exactly "rates-difference") $
                                   M.insert "source"   (Optional source) attrs
                          qry = CQuery {
                                  cqType       = differenceType diffD,
                                  -- Search for accounts only in default currency
                                  cqCurrencies = [defcur],
                                  cqExcept     = accounts,
                                  cqAttributes = attrs' }
                      correspondence <- lookupCorrespondence qry (diffD :# defcur) Nothing
                      case correspondence of
                        Right oneAccount -> do
                          if diffD < 0
                            then do
                                 account <- accountAsCredit oneAccount
                                 return $ CreditDifference $ CPosting account (-diffD)
                            else do
                                 account <- accountAsDebit oneAccount
                                 return $ DebitDifference [ DPosting account diffD ]
                        Left debitPostings -> do
                          postings <- mapM convertPosting' debitPostings
                          return $ DebitDifference postings
         return $ CEntry dtF crF rd
    else return $ CEntry dtF crF OneCurrency

-- | Lookup for correspondent account(s).
-- If needed amount could be wrote off from one corresponding account,
-- return that account. Otherwise, return list of debit postings
-- (with different amounts and accounts).
--
-- For example, if there is account `A1' with debit redirect set up,
-- and it's current balance is $100, but we need to debit it by $500,
-- then it will return at least two postings: debit A1 by $100,
-- debit A2 by $400, where A2 is some another account.
lookupCorrespondence :: (Throws NoCorrespondingAccountFound l,
                         Throws NoSuchRate l,
                         Throws InternalError l,
                         Throws InvalidAccountType l)
                     => CQuery              -- ^ Query to search for account
                     -> Amount              -- ^ Amount of credit \/ debit
                     -> Maybe AnyAccount    -- ^ User-specified corresponding account
                     -> Ledger l (Either [Posting Amount Debit] AnyAccount)
lookupCorrespondence qry amount@(value :# currency) mbCorr = do
  coa <- gets lsCoA
  amap <- gets lsAccountMap
  let mbAccount = runCQuery qry coa
      mbByMap = lookupAMap coa amap qry (cqExcept qry)
  case mbCorr `mplus` mbByMap `mplus` mbAccount of
    Nothing -> throwP (NoCorrespondingAccountFound qry)
    Just acc ->
      case cqType qry of
        ECredit -> return (Right acc)
        EDebit ->
          -- Check if we should redirect part of debit
          case acc of
            -- Debit redirect has sence only for free accounts
            WFree _ account ->
              if not (freeAccountRedirect account)
                then return (Right acc)
                else do
                     let accountCurrency = getCurrency account
                     toDebit :# _ <- convert accountCurrency amount
                     currentBalance <- getCurrentBalance account
                     -- toDebit and currentBalance are both in currency of
                     -- found account.
                     let firstPosting = DPosting (Left account) (currentBalance :# accountCurrency)
                     if toDebit <= currentBalance
                       then return $ Left [firstPosting]
                       else do
                            let toRedirect = toDebit - currentBalance
                            let qry' = CQuery {
                                         -- Search for debit (or free) account
                                         cqType = EDebit,
                                         -- If possible, use account with same currency
                                         cqCurrencies = (accountCurrency: cqCurrencies qry),
                                         -- Do not use same account or one of
                                         -- account that already are used in this entry
                                         cqExcept     = (getID account: cqExcept qry),
                                         cqAttributes = M.insert "redirectedFrom"
                                                          (Exactly $ getName account)
                                                          (cqAttributes qry)
                                       }
                            nextHop <- lookupCorrespondence qry'
                                           (toRedirect :# accountCurrency)
                                           Nothing
                            case nextHop of
                              Right hop -> do
                                -- We can debit `hop' acccount by toRedirect
                                let lastCurrency = getCurrency hop
                                lastDebit :# _ <- convert lastCurrency (toRedirect :# accountCurrency)
                                hop' <- accountAsDebit hop
                                let last  = DPosting hop' (lastDebit :# lastCurrency)
                                return $ Left [firstPosting, last]
                              Left postings -> 
                                return $ Left (firstPosting: postings)
            -- For other account types, just return found account.
            _ -> return (Right acc)
            

-- | Fill in debit and credit parts of entry, so that
-- credit - debit == 0.
fillEntry :: (Throws NoSuchRate l,
              Throws NoCorrespondingAccountFound l,
              Throws InvalidAccountType l,
              Throws InternalError l)
           => CQuery                    -- ^ Query to search for corresponding account
           -> [Posting Decimal Debit]   -- ^ Debit postings
           -> [Posting Decimal Credit]  -- ^ Credit postings
           -> Maybe AnyAccount          -- ^ User-specified corresponding account
           -> Amount                    -- ^ Difference (credit - debit)
           -> Ledger l ([Posting Decimal Debit], [Posting Decimal Credit])
fillEntry qry dt cr mbCorr amount@(value :# _) = do
  correspondence <- lookupCorrespondence qry amount mbCorr
  case correspondence of
    Right oneAccount -> do
      if value < 0
         then do
              account <- accountAsCredit oneAccount
              -- Convert value into currency of found account
              value' :# _ <- convert (getCurrency account) amount
              let e = CPosting account (-value')
              -- Will fill rates difference later
              return (dt, e:cr)
         else do
              account <- accountAsDebit oneAccount
              -- Convert value into currency of found account
              value' :# _ <- convert (getCurrency account) amount
              let e = DPosting account value'
              -- Will fill rates difference later
              return (e:dt, cr)
    Left debitPostings -> do
      postings <- mapM convertPosting' debitPostings
      return (dt ++ postings, cr)

-- | Reconciliate one account; set it's current balance
-- to given value.
reconciliate :: (Throws NoSuchRate l,
                 Throws InvalidAccountType l,
                 Throws InternalError l)
             => DateTime       -- ^ Transaction date/time
             -> AnyAccount     -- ^ Account
             -> Amount         -- ^ Balance value to set
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

-- | Calculate sum of amounts in accounts group.
sumGroup :: (Throws InternalError l,
             Throws NoSuchRate l)
         => AccountGroupData
         -> [Amount]
         -> Ledger l Amount
sumGroup ag ams = do
  setPos $ newPos ("accounts group " ++ agName ag) 0 0
  let c = agCurrency ag
  ams' <- mapM (convert c) ams
  let res = sum [x | x :# _ <- ams']
  return $ res :# c

-- | Calculate saldos for each account \/ group in CoA.
treeSaldo :: (Throws InternalError l,
              Throws NoSuchRate l)
          => Query
          -> ChartOfAccounts
          -> Ledger l (Tree Amount Amount)
treeSaldo qry coa = mapTreeM sumGroup (saldo qry) coa

