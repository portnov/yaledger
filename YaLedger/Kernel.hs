{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
-- | Ledger kernel
module YaLedger.Kernel
  (module YaLedger.Kernel.Common,
   CanCredit (..), CanDebit (..),
   negateAmount, differenceType,
   getCurrentBalance,
   convert, convertPosting,
   convertPosting', convertDecimal,
   checkQuery, checkRecord, isAdmin,
   creditPostings, debitPostings,
   accountByID,
   sumGroup, sumPostings,
   accountAsCredit,
   accountAsDebit,
   checkEntry,
   reconciliate,
   creditTurnovers, debitTurnovers,
   saldo, treeSaldo
  ) where

import Prelude hiding (catch)
import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Data.Maybe
import Data.List
import Data.Dates
import Data.Decimal
import qualified Data.Map as M

import YaLedger.Types
import YaLedger.Monad
import YaLedger.Exceptions
import YaLedger.Correspondence
import YaLedger.Kernel.Common
import YaLedger.Logger

-- | Accounts that could be credited
class CanCredit a where
  credit :: (Throws InternalError l,
             Throws InsufficientFunds l)
         => a
         -> Ext (Posting Decimal Credit)
         -> Ledger l ()

-- | Accounts that could be debited
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

-- | Add posting value to balances history
balancePlus :: forall l t.
               (Throws InternalError l, Sign t)
            => Ext (Posting Decimal t)   -- ^ Posting
            -> History Balance Checked   -- ^ Balances history
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

-- | Get current balance of account.
-- Value is returned in currency of account.
getCurrentBalance :: (IsAccount a,
                      Throws InternalError l)
                  => a                 -- ^ Any sort of account
                  -> Ledger l Decimal
getCurrentBalance acc = do
  balances <- readIOList (accountBalances acc)
  return $ case balances of
             [] -> 0
             (b:_) -> balanceValue (getContent b)

-- | Check if balance account would be OK.
-- Issue INFO:, WARNING:, or exception.
checkBalance :: (Throws InternalError l,
                 Throws InsufficientFunds l,
                 IsAccount a)
             => Decimal        -- ^ Account balance
             -> a              -- ^ Any sort of account
             -> Ledger l ()
checkBalance targetBalance acc = do
  let bc = accountChecks acc
      op = if sign acc > 0
             then (>=)
             else (<=)
  whenJust (bcInfo bc) $ \value ->
    when (targetBalance `op` value) $
      info $ "Balance of " ++ getName acc ++ " will be " ++ show targetBalance ++ show (getCurrency acc)
  whenJust (bcWarning bc) $ \value ->
    when (targetBalance `op` value) $ 
      warning $ "Balance of " ++ getName acc ++ " will be " ++ show targetBalance ++ show (getCurrency acc)
  whenJust (bcError bc) $ \value ->
    when (targetBalance `op` value) $ 
      throwP (InsufficientFunds (getName acc) targetBalance (getCurrency acc))

negateAmount :: Amount -> Amount
negateAmount (x :# c) = (-x) :# c

differenceType :: Decimal -> PostingType
differenceType x
  | x < 0     = ECredit
  | otherwise = EDebit

-- | Lookup for active exchange rate
lookupRate :: (Throws NoSuchRate l)
           => Maybe DateTime    -- ^ Date to search exchange rate for. Nothing for current date.
           -> Currency          -- ^ Source currency
           -> Currency          -- ^ Target currency
           -> Ledger l Double
lookupRate mbDate from to = do
    now <- gets lsStartDate
    let date = fromMaybe now mbDate
    rates <- gets lsRates
    let goodRates = [getContent rate | rate <- rates, getDate rate <= date]
    case go goodRates from to goodRates of
      Nothing -> throwP (NoSuchRate from to)
      Just rate -> return rate
  where
    go _ _ _ [] = Nothing
    go ar f t (Explicit cFrom aFrom cTo aTo rev: rs)
      | (cFrom == f) && (cTo == t) = Just (aTo / aFrom)
      | rev && (cTo == f) && (cFrom == t) = Just (aFrom / aTo)
      | otherwise = go ar f t rs
    go ar f t (Implicit cFrom cTo cBase rev: rs)
      | (cFrom == f) && (cTo == t) = do
            x <- go ar f cBase ar
            y <- go ar cBase t ar
            return (x / y)
      | rev && (cFrom == t) && (cTo == f) = do
            x <- go ar t cBase ar
            y <- go ar cBase f ar
            return (x / y)
      | otherwise = go ar f t rs

-- | Convert 'Amount' to another currency
convert :: (Throws NoSuchRate l)
        => Maybe DateTime    -- ^ Date of which exchange rate should be used
        -> Currency          -- ^ Target currency
        -> Amount
        -> Ledger l Amount
convert mbDate c' (x :# c)
  | c == c' = return (x :# c)
  | otherwise = do
    rate <- lookupRate mbDate c c'
    -- Round amount to precision of target currency
    let qty = roundTo (fromIntegral $ cPrecision c') (x *. rate)
    return $ qty :# c'

-- | Check if record \/ entry \/ whatever matches to query
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

-- | Similar to 'checkQuery', but this is True for all
-- admin records
checkRecord :: Query -> Ext Record -> Bool
checkRecord qry rec =
    isAdmin (getContent rec) || checkQuery qry rec

-- | Check if record is administrative
-- (non-financial)
isAdmin :: Record -> Bool
isAdmin (Transaction _) = False
isAdmin _               = True

-- | Get history of account's credit postings
creditPostings :: Throws InternalError l
               => AnyAccount
               -> Ledger l (History (Posting Decimal) Credit)
creditPostings (WCredit _ (CAccount {..})) = return creditAccountPostings
creditPostings (WDebit  _ (DAccount {..})) = newIOList
creditPostings (WFree   _ (FAccount {..})) = return freeAccountCreditPostings

-- | Get history of account's debit postings
debitPostings :: Throws InternalError l
              => AnyAccount
              -> Ledger l (History (Posting Decimal) Debit)
debitPostings (WCredit _ (CAccount {..})) = newIOList
debitPostings (WDebit  _ (DAccount {..})) = return debitAccountPostings
debitPostings (WFree   _ (FAccount {..})) = return freeAccountDebitPostings

filterPostings :: Query -> [Ext (Posting Decimal t)] -> [Posting Decimal t]
filterPostings query list = map getContent $ filter (checkQuery query) list

-- | Calculate account saldo
saldo :: (Throws InternalError l)
      => Query              -- ^ Query to select postings
      -> AnyAccount         -- ^ Account
      -> Ledger l Amount
saldo qry account = do
  credit :# c <- creditTurnovers qry account
  debit  :# _ <- debitTurnovers  qry account
  return $ (credit - debit) :# c

-- | Calculate credit turnovers of account
-- (sum of credit postings)
creditTurnovers :: (Throws InternalError l)
                => Query            -- ^ Query to select postings
                -> AnyAccount       -- ^ Account
                -> Ledger l Amount
creditTurnovers qry account = do
  let c = getCurrency account
  postings <- filterPostings qry <$> (readIOList =<< creditPostings account)
  let turnovers = sumPostings postings
  return (turnovers :# c)

-- | Calculate debit turnovers of account
-- (sum of debit postings)
debitTurnovers :: (Throws InternalError l)
               => Query            -- ^ Query to select postings
               -> AnyAccount       -- ^ Account
               -> Ledger l Amount
debitTurnovers qry account = do
  let c = getCurrency account
  postings <- filterPostings qry <$> (readIOList =<< debitPostings account)
  let turnovers = sumPostings postings
  return (turnovers :# c)

-- | Sum values of postings.
-- NB: does not use currencies. This should be
-- used only for postings that are known to be 
-- in one currency.
sumPostings :: [Posting Decimal t] -> Decimal
sumPostings es = sum (map postingValue es)

first :: (a -> Maybe b) -> [a] -> Maybe b
first _ [] = Nothing
first fn (x:xs) =
    case fn x of
      Just y  -> Just y
      Nothing -> first fn xs

-- | Lookup for account by it's ID
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

-- | Return same account as FreeOr Credit Account,
-- or throw an exception.
accountAsCredit :: (Throws InvalidAccountType l)
                => AnyAccount
                -> Ledger l (FreeOr Credit Account)
accountAsCredit (WDebit  _ a) = throwP $ InvalidAccountType (getName a) AGDebit AGCredit
accountAsCredit (WCredit _ a) = return $ Right a
accountAsCredit (WFree   _ a) = return $ Left a

-- | Return same account as FreeOr Debit Account,
-- or throw an exception.
accountAsDebit :: (Throws InvalidAccountType l)
                => AnyAccount
                -> Ledger l (FreeOr Debit Account)
accountAsDebit (WCredit _ a) = throwP $ InvalidAccountType (getName a) AGCredit AGDebit
accountAsDebit (WDebit  _ a) = return $ Right a
accountAsDebit (WFree   _ a) = return $ Left a

-- | Similar to Data.List.nub, but removes ALL duplications.
-- O(n^2).
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x:xs) = x: uniq (filter (/= x) xs)

-- | Convert a posting to another currency.
-- In returned posting, amount is in target currency.
convertPosting :: Throws NoSuchRate l
               => Maybe DateTime      -- ^ Date of exchange rates
               -> Currency            -- ^ Target currency
               -> Posting Amount t
               -> Ledger l (Posting Decimal t)
convertPosting mbDate to (DPosting acc a) = do
  x :# _ <- convert mbDate to a
  return $ DPosting acc x
convertPosting mbDate to (CPosting acc a) = do
  x :# _ <- convert mbDate to a
  return $ CPosting acc x

-- | Convert a posting to currency of it's account.
convertPosting' :: Throws NoSuchRate l
               => Maybe DateTime        -- ^ Date of exchange rates
               -> Posting Amount t
               -> Ledger l (Posting Decimal t)
convertPosting' mbDate (DPosting acc a) = do
  x :# _ <- convert mbDate (getCurrency acc) a
  return $ DPosting acc x
convertPosting' mbDate (CPosting acc a) = do
  x :# _ <- convert mbDate (getCurrency acc) a
  return $ CPosting acc x

-- | Convert Posting Decimal. Returns only an amount in target currency.
convertDecimal :: Throws NoSuchRate l
               => Maybe DateTime
               -> Currency
               -> Posting Decimal t
               -> Ledger l Decimal
convertDecimal mbDate to (DPosting acc a) = do
  x :# _ <- convert mbDate to (a :# getCurrency acc)
  return x
convertDecimal mbDate to (CPosting acc a) = do
  x :# _ <- convert mbDate to (a :# getCurrency acc)
  return x

setZero :: Posting Decimal t -> Posting Decimal t
setZero (CPosting a _) = CPosting a 0
setZero (DPosting a _) = DPosting a 0

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
             => DateTime                 -- ^ Entry date/time
             -> Attributes               -- ^ Transaction attributes
             -> Entry Amount Unchecked   -- ^ Unchecked entry
             -> Ledger l (Entry Decimal Checked)
checkEntry date attrs (UEntry dt cr mbCorr currs) = do
  defcur <- gets lsDefaultCurrency
  let currencies    = uniq $ map getCurrency cr ++ map getCurrency dt ++ [defcur]
      firstCurrency = head currencies
      accounts      = map (getID . creditPostingAccount) cr
                   ++ map (getID . debitPostingAccount) dt
      accountNames  = map (getName . creditPostingAccount) cr
                   ++ map (getName . debitPostingAccount) dt
      source        = head accountNames

  -- Convert all postings into firstCurrency
  dt1 <- mapM (convertPosting (Just date) firstCurrency) dt
  cr1 <- mapM (convertPosting (Just date) firstCurrency) cr

  -- And sum them to check if debit == credit
  let dtSum = sumPostings dt1 
      crSum = sumPostings cr1

  -- Convert each posting's sum into currency
  -- of posting's account
  dt' <- mapM (convertPosting' $ Just date) dt
  cr' <- mapM (convertPosting' $ Just date) cr

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
                       fillEntry qry date dt' cr' mbCorr (diff :# firstCurrency)
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
         crD <- mapM (convertDecimal (Just date) defcur) crF
         dtD <- mapM (convertDecimal (Just date) defcur) dtF
         debug $ "crD: " ++ show crD ++ ", dtD: " ++ show dtD

         let diffD :: Decimal -- In default currency
             diffD = sum crD - sum dtD
         rd <- if diffD == realFracToDecimal 10 (fromIntegral 0)
                 then return OneCurrency
                 else do
                      debug $ "Rates difference: " ++ show diffD
                      let attrs' = M.insert "category" (Exactly "rates-difference") $
                                   M.insert "source"   (Optional source) attrs
                          qry = CQuery {
                                  cqType       = differenceType diffD,
                                  -- Search for accounts only in default currency
                                  cqCurrencies = [defcur],
                                  cqExcept     = accounts,
                                  cqAttributes = attrs' }
                      correspondence <- lookupCorrespondence qry date (diffD :# defcur) Nothing
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
                          postings <- mapM (convertPosting' $ Just date) debitPostings
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
                     -> DateTime            -- ^ Entry date/time
                     -> Amount              -- ^ Amount of credit \/ debit
                     -> Maybe AnyAccount    -- ^ User-specified corresponding account
                     -> Ledger l (Either [Posting Amount Debit] AnyAccount)
lookupCorrespondence qry date amount@(value :# currency) mbCorr = do
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
                     toDebit :# _ <- convert (Just date) accountCurrency amount
                     currentBalance <- getCurrentBalance account
                     -- toDebit and currentBalance are both in currency of
                     -- found account.
                     let firstPosting = DPosting (Left account) (currentBalance :# accountCurrency)
                     if toDebit <= currentBalance
                       then return $ Left [DPosting (Left account) (toDebit :# accountCurrency)]
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
                            nextHop <- lookupCorrespondence qry' date
                                           (toRedirect :# accountCurrency)
                                           Nothing
                            case nextHop of
                              Right hop -> do
                                -- We can debit `hop' acccount by toRedirect
                                let lastCurrency = getCurrency hop
                                lastDebit :# _ <- convert (Just date) lastCurrency (toRedirect :# accountCurrency)
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
           -> DateTime                  -- ^ Entry date/time
           -> [Posting Decimal Debit]   -- ^ Debit postings
           -> [Posting Decimal Credit]  -- ^ Credit postings
           -> Maybe AnyAccount          -- ^ User-specified corresponding account
           -> Amount                    -- ^ Difference (credit - debit)
           -> Ledger l ([Posting Decimal Debit], [Posting Decimal Credit])
fillEntry qry date dt cr mbCorr amount@(value :# _) = do
  correspondence <- lookupCorrespondence qry date amount mbCorr
  case correspondence of
    Right oneAccount -> do
      if value < 0
         then do
              account <- accountAsCredit oneAccount
              -- Convert value into currency of found account
              value' :# _ <- convert (Just date) (getCurrency account) amount
              if value' == 0
                then if (length dt == 1) && null cr
                       then do
                         info $ "Credit part is zero: " ++ show cr ++ ", setting debit to zero too." 
                         return (map setZero dt, cr)
                       else do
                         warning $ "Credit-Debit = " ++ show amount ++ ", which is 0.0"
                                 ++ show (getCurrency account)
                         return (dt, cr)
                else do
                     let e = CPosting account (-value')
                     return (dt, e:cr)
         else do
              account <- accountAsDebit oneAccount
              -- Convert value into currency of found account
              value' :# _ <- convert (Just date) (getCurrency account) amount
              if value' == 0
                then if (length cr == 1) && null dt
                       then do
                         info $ "Debit part is zero: " ++ show dt ++ ", setting credit to zero too." 
                         return (dt, map setZero cr)
                       else do
                         warning $ "Credit-Debit = " ++ show amount ++ ", which is 0.0"
                                 ++ show (getCurrency account)
                         return (dt, cr)
                else do
                     let e = DPosting account value'
                     return (e:dt, cr)
    Left debitPostings -> do
      let diffValue = sum [x | x :# _ <- map postingValue debitPostings]
      postings <- mapM (convertPosting' $ Just date) debitPostings
      let value = sum (map postingValue postings)
      if value == 0
        then do
             if (length cr == 1) && null dt
               then do
                 info $ "Debit is " ++ show diffValue ++
                        ", which is 0.0 in currencies of amounts. Setting credit to zero too."
                 return (dt, map setZero cr)
               else do
                 warning $ "Debit difference is " ++ show diffValue ++
                        ", which is 0.0 in currencies of amounts."
                 return (dt, cr)
        else return (dt ++ postings, cr)

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
              qStart      = Nothing,
              qEnd        = Just date,
              qAllAdmin   = True,
              qAttributes = M.empty }

  currentBalance :# accountCurrency <- saldo qry account
  actualBalance :# _ <- convert (Just date) (getCurrency account) amount

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
         => Maybe DateTime
         -> AccountGroupData
         -> [Amount]
         -> Ledger l Amount
sumGroup mbDate ag ams = do
  setPos $ newPos ("accounts group " ++ agName ag) 0 0
  let c = agCurrency ag
  ams' <- mapM (convert mbDate c) ams
  let res = sum [x | x :# _ <- ams']
  return $ res :# c

-- | Calculate saldos for each account \/ group in CoA.
treeSaldo :: (Throws InternalError l,
              Throws NoSuchRate l)
          => Query
          -> ChartOfAccounts
          -> Ledger l (Tree Amount Amount)
treeSaldo qry coa = mapTreeM (sumGroup $ qEnd qry) (saldo qry) coa

