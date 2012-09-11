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
import Text.Printf

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

creditPostings :: AnyAccount -> [Ext (Posting Amount Credit)]
creditPostings (WCredit _ (CAccount {..})) = creditAccountPostings
creditPostings (WDebit  _ (DAccount {..})) = []
creditPostings (WFree   _ (FAccount {..})) = freeAccountCreditPostings

debitPostings :: AnyAccount -> [Ext (Posting Amount Debit)]
debitPostings (WCredit _ (CAccount {..})) = []
debitPostings (WDebit  _ (DAccount {..})) = debitAccountPostings
debitPostings (WFree   _ (FAccount {..})) = freeAccountDebitPostings

saldo :: (Throws NoSuchRate l)
      => Query -> AnyAccount -> Ledger l Amount
saldo query account = do
  rs <- gets lsRates
  let c = getCurrency account
  cr :# _ <- sumPostings c $ map getContent $ filter (checkQuery query) $ creditPostings account
  dt :# _ <- sumPostings c $ map getContent $ filter (checkQuery query) $ debitPostings  account
  return $ (cr - dt) :# c

sumPostings :: (Throws NoSuchRate l)
           => Currency -> [Posting Amount t] -> Ledger l Amount
sumPostings c es = do
    rs <- gets lsRates
    ams <- mapM (convert c) (map getAmount es)
    let s = sum [x | x :# _ <- ams]
    return (s :# c)

accountByID :: AccountID t -> AccountPlan -> Maybe AnyAccount
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
                => AccountID t
                -> Ledger l ()
accountIDIsCredit aid = accountIsCredit =<< accountByIDM aid

accountIDIsDebit :: (Throws InvalidAccountType l)
                => AccountID t
                -> Ledger l ()
accountIDIsDebit aid = accountIsDebit =<< accountByIDM aid

accountByIDM :: AccountID t -> Ledger l AnyAccount
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
                 Throws InvalidAccountType l)
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
                 Throws InvalidAccountType l)
             => DateTime
             -> AccountID t
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
  message $ "Debit postings: " ++ show (debitPostings account)
  message $ "Credit postings: " ++ show (creditPostings account)
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

updateAccount :: Integer
              -> AccountPlan
              -> (AnyAccount -> Ledger l AnyAccount)
              -> Ledger l AccountPlan
updateAccount i leaf@(Leaf _ _ acc) fn
  | getID acc == i = do
                     message $ "Account found: #" ++ show i
                     acc' <- fn acc
                     message $ "Credit postings: " ++ show (creditPostings acc')
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

