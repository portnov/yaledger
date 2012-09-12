{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Kernel where

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

class CanCredit t where
  credit :: Throws InternalError l
         => Account t
         -> Ext (Posting Amount Credit)
         -> Ledger l ()

class CanDebit t where
  debit :: Throws InternalError l
        => Account t
        -> Ext (Posting Amount Debit)
        -> Ledger l ()

instance CanDebit Debit where
  debit (DAccount {..}) p =
      appendIOList debitAccountPostings p

instance CanDebit Free where
  debit (FAccount {..}) p =
      appendIOList freeAccountDebitPostings p

instance CanCredit Credit where
  credit (CAccount {..}) p =
      appendIOList creditAccountPostings p

instance CanCredit Free where
  credit (FAccount {..}) p =
      appendIOList freeAccountCreditPostings p

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

creditPostings :: Throws InternalError l
               => AnyAccount
               -> Ledger l (IOList (Ext (Posting Amount Credit)))
creditPostings (WCredit _ (CAccount {..})) = return creditAccountPostings
creditPostings (WDebit  _ (DAccount {..})) = newIOList
creditPostings (WFree   _ (FAccount {..})) = return freeAccountCreditPostings

debitPostings :: Throws InternalError l
              => AnyAccount
              -> Ledger l (IOList (Ext (Posting Amount Debit)))
debitPostings (WCredit _ (CAccount {..})) = newIOList
debitPostings (WDebit  _ (DAccount {..})) = return debitAccountPostings
debitPostings (WFree   _ (FAccount {..})) = return freeAccountDebitPostings

saldo :: (Throws NoSuchRate l,
          Throws InternalError l)
      => Query -> AnyAccount -> Ledger l Amount
saldo query account = do
  rs <- gets lsRates

  let c = getCurrency account

      filterPostings :: [Ext (Posting Amount t)] -> [Posting Amount t]
      filterPostings list = map getContent $ filter (checkQuery query) list

  crp <- filterPostings <$> (readIOList =<< creditPostings account)
  dtp <- filterPostings <$> (readIOList =<< debitPostings  account)
  cr :# _ <- sumPostings c crp
  dt :# _ <- sumPostings c dtp
  return $ (cr - dt) :# c

sumPostings :: (Throws NoSuchRate l)
           => Currency -> [Posting Amount t] -> Ledger l Amount
sumPostings c es = do
    rs <- gets lsRates
    ams <- mapM (convert c) (map getAmount es)
    let s = sum [x | x :# _ <- ams]
    return (s :# c)

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

accountIsCredit :: (Throws InvalidAccountType l)
                => AnyAccount
                -> Ledger l ()
accountIsCredit (WDebit _ _) = throw $ InvalidAccountType AGDebit AGCredit
accountIsCredit _            = return ()

accountIsDebit :: (Throws InvalidAccountType l)
                => AnyAccount
                -> Ledger l ()
accountIsDebit (WCredit _ _) = throw $ InvalidAccountType AGCredit AGDebit
accountIsDebit _             = return ()

accountIDIsCredit :: (Throws InvalidAccountType l)
                => AccountID
                -> Ledger l ()
accountIDIsCredit aid = accountIsCredit =<< accountByIDM aid

accountIDIsDebit :: (Throws InvalidAccountType l)
                => AccountID
                -> Ledger l ()
accountIDIsDebit aid = accountIsDebit =<< accountByIDM aid

accountByIDM :: AccountID -> Ledger l AnyAccount
accountByIDM aid = do
  plan <- gets lsAccountPlan
  case accountByID aid plan of
   Nothing -> fail $ "Internal error: no such account: " ++ show aid
   Just acc -> return acc

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x:xs) = x: uniq (filter (/= x) xs)

checkEntry :: (Throws NoSuchRate l,
                 Throws NoCorrespondingAccountFound l,
                 Throws InvalidAccountType l,
                 Throws InternalError l)
             => Attributes
             -> Entry Amount Unchecked
             -> Ledger l (Entry Amount Checked)
checkEntry attrs src@(UEntry dt cr mbCorr) = do
  rs <- gets lsRates
  plan <- gets lsAccountPlan
  amap <- gets lsAccountMap
  defcur <- gets lsDefaultCurrency
  let currencies = uniq $ map getCurrency cr ++ map getCurrency dt ++ [defcur]
      firstCurrency = head currencies
      accounts = map creditPostingAccount cr ++ map debitPostingAccount dt
  dtSum :# _ <- sumPostings firstCurrency dt
  crSum :# _ <- sumPostings firstCurrency cr
  if dtSum == crSum
    then return $ CEntry dt cr
    else do
         message $ printf "cr. %s, dt. %s" (show crSum) (show dtSum)
         let diff = crSum - dtSum
             qry = CQuery {
                     cqType = if diff < 0
                                then ECredit
                                else EDebit,
                     cqCurrency = currencies,
                     cqAttributes = attrs }
         let mbAccount = runCQuery qry plan
             mbByMap = lookupAMap plan amap qry accounts
         case mbCorr `mplus` mbByMap `mplus` mbAccount of
           Nothing -> throw (NoCorrespondingAccountFound qry)
           Just acc -> if diff < 0
                         then do
                              message $ "Corresponding account for " ++ show src ++ ": " ++ show acc
                              accountIsCredit acc
                              let e = CPosting (getID acc) (-diff :# firstCurrency)
                              return $ CEntry dt (e:cr)
                         else do
                              accountIsDebit acc
                              message $ "Corresponding account for " ++ show src ++ ": " ++ show acc
                              let e = DPosting (getID acc) (diff :# firstCurrency)
                              return $ CEntry (e:dt) cr

reconciliate :: (Throws NoSuchRate l,
                 Throws InvalidAccountType l,
                 Throws InternalError l)
             => DateTime
             -> AccountID
             -> Amount
             -> Ledger l (Entry Amount Unchecked)
reconciliate date aid (x :# c) = do
  account <- accountByIDM aid
  let qry = Query {
              qStart = Nothing,
              qEnd   = Just date,
              qAttributes = [] }
  bal <- saldo qry account
  (currentBalance :# _) <- convert c bal
  message $ "Account: " ++ show account
  message $ "Current balance: " ++ show bal
  let diff = x - currentBalance
  if diff > 0
    then do
         accountIsCredit account
         let posting = CPosting (getID account) (diff :# c)
         return $ UEntry [] [posting] Nothing
    else do
         accountIsDebit account
         let posting = DPosting (getID account) (-diff :# c)
         return $ UEntry [posting] [] Nothing

updateAccount :: Throws InternalError l
              => Integer
              -> AccountPlan
              -> (AnyAccount -> Ledger l ())
              -> Ledger l ()
updateAccount i (Leaf _ _ acc) fn
  | getID acc == i = do
                     message $ "Account found: #" ++ show i
                     fn acc
  | otherwise      = return ()
updateAccount i (Branch _ _ ag children) fn
  | i `inRange` agRange ag = mapM_ (\c -> updateAccount i c fn) children
  | otherwise              = return ()

updatePlan :: (AccountPlan -> Ledger l ()) -> Ledger l ()
updatePlan fn = do
  st <- get
  let plan = lsAccountPlan st
  fn plan

