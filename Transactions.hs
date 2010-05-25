{-# LANGUAGE UnicodeSyntax, TypeOperators, TypeSynonymInstances, PatternGuards, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Transactions where

import Control.Monad
import Control.Monad.State.Class
import Text.Regex.PCRE hiding (match)
import qualified Data.Map as M
import Data.List

import Types
import Unicode
import Dates
import Lists
import qualified Tree as T
import Currencies
import Accounts

import Debug.Trace

isNotPR ∷ Dated Record → Bool
isNotPR (At _ (PR _)) = False
isNotPR _             = True

filterRecords ∷ Conditions → [Dated Record] → [Dated Record]
filterRecords cs recs = filterUnsorted unsorted (filterSorted sorted recs)
  where
    cs' = conditions cs
    sorted = filter areRecordsSortedBy cs'
    unsorted = filter (not ∘ areRecordsSortedBy) cs'

filterSorted ∷ [Condition] → [Dated Record] → [Dated Record]
filterSorted cs recs = takeWhile pred' □ dropWhile (not ∘ pred') recs
  where
    pred = foldl (liftM2 (∧)) (const True) [match param | Condition match param _ ← cs]
    pred' ∷ Dated Record → Bool
    pred' r = (pred r) ∨ (isNotPR r)

filterUnsorted ∷ [Condition] → [Dated Record] → [Dated Record]
filterUnsorted cs recs = filter pred' recs
  where
    pred = foldl (liftM2 (∧)) (const True) [match param | Condition match param _ ← cs]
    pred' ∷ Dated Record → Bool
    pred' r = (pred r) ∨ (isNotPR r)

datedSeq ∷ Dated a → DateInterval → [Dated a]
datedSeq (At dt x) int = [At d x | d ← datesFromEvery dt int]

regularToList ∷ RegularTransaction → [Dated Record]
regularToList (RegularTransaction date int post) = datedSeq (At date □ PR post) int

allToList ∷ Conditions → [Dated Record] → [Dated Record]
allToList pred recs = filterRecords pred □ mergeOn getDate (ones : map toList regs)
  where
    isReg ∷ Dated Record → Bool
    isReg (At _ (RegR _)) = True
    isReg _               = False

    regs ∷ [Dated Record]
    regs = filter isReg recs

    ones ∷ [Dated Record]
    ones = filter (not.isReg) recs

    toList ∷ Dated Record → [Dated Record]
    toList (At _ (RegR reg)) = regularToList reg
    toList x = [x]

amountsList ∷ Transaction → [Name] → [Maybe Amount]
amountsList (Transaction _ _ posts) accs = 
  let lst = [(last □ T.mkPath □ getAccountName p, getAmount p) | p ← posts]
  in  map (\a → lookup a lst) accs

amountsList' ∷ Dated Record → [Name] → [Maybe Amount]
amountsList' (At _ (PR tr)) accs = amountsList tr accs

fixPostings ∷ Rates → [Posting] → Posting → [Posting]
fixPostings rates parts (Auto acc) = parts ⧺ [acc :<+ (F amount)]
  where
    amount = negateAmount □ sumAmounts rates □ map getAmount parts

checkPostings ∷ Rates → [Posting] → LState [Posting]
checkPostings rates parts = 
  if (getValue □ sumAmounts rates □ map getAmount parts) ≡ 0.0
    then return parts
    else fail "Transaction does not balance"

putAmount ∷ Name → Amount → LState ()
putAmount name (x :# c) = do
  st ← get
  let accs = accounts st
      rs   = rates st
      dt   = now st
  acc ← getAccount name
  let y = convert rs x c (accCurrency acc)
      acc' = acc {history = (history acc) ⧺ [(dt,y)]}

  let hld = hold acc
      checkHold = (getValue hld ≠ 0.0)
      y = convert rs x c (accCurrency acc)

  when checkHold □ do
      let was = sumAccount acc
          will = amountPlus rs was (x :# c)
      b ← amountLT will hld
      when b □ fail □ "Hold violation for " ⧺ accName acc ⧺ ": hold " ⧺ show hld ⧺ ", will " ⧺ show will

  let acc' = acc {history = (history acc) ⧺ [(dt,y)]}
      m = T.changeLeaf accs (T.mkPath name) acc'
  put □ st {accounts = m}

doPosting ∷ Posting → LState ()
doPosting (acc :<+ amount) = putAmount acc (defAmount amount)

checkTransaction ∷ Transaction → LState Transaction
checkTransaction post = do
  let srcPostings = parts post
  rs ← gets rates
  parts' ← forM srcPostings □ \p → do
              case p of
                name :<+ (F (x :# "%")) → do
                  acc ← getAccount name
                  let (s :# c) = sumAccount acc
                      y = s*x/100.0
                  return □ name :<+ (F (y :# c))
                p' → return p'
  parts'' ← case filter isAuto parts' of
              [p] → return □ fixPostings rs (filter (not ∘ isAuto) parts') p
              []  → checkPostings rs parts'
              _   → fail "More than one lines without amount given"
  return □ post {parts = parts''}

doTransaction ∷ Transaction → LState ()
doTransaction post = do
  post' ← checkTransaction post
  forM_ (parts post') doPosting

getTemplate ∷ String → LState Template
getTemplate name = do
  tpls ← gets templates
  case M.lookup name tpls of
    Nothing → fail □ "Unknown template: " ⧺ name
    Just tpl → return tpl

putRecord ∷ Dated Record → LState ()
putRecord rr | At dt (PR post) ← rr = do
    post' ← checkTransaction post
    modify (add □ At dt (PR post'))
             | otherwise = modify (add rr)
  where
    add r st = st {records = records st ⧺ [r]}

getAccounts ∷ Transaction → [(String,Amount)]
getAccounts post = map convert □ parts post
  where
    convert (acc :<+ a) = (acc, defAmount a)
    convert (Auto a) = error □ "Internal error: unexpected Auto posting-part: " ⧺ a

getAmounts ∷ Transaction → [Amount]
getAmounts post = concatMap get (parts post)
  where
    get (_ :<+ a) = [defAmount a]
    get (Auto _)  = []

matchName ∷ String → (String,Amount) → Bool
matchName regex (name,_) = name =~ regex

match ∷ Rule → Transaction → LState Bool
match (DescrMatch regex) post = return □ (description post) =~ regex
match (regex :> amount) post = orM □ map match' □ filter (matchName regex) □ getAccounts post
  where
    match' (acc,amount') = amountGT amount' amount
match (regex :< amount) post = orM □ map match' □ filter (matchName regex) □ getAccounts post
  where
    match' (acc,amount') = amountLT amount' amount

applyRules ∷ Transaction → LState [Transaction]
applyRules post = do
  rls ← gets ruled
  res ← forM rls □ \(when,rule,post') → do
    m ← match rule post
    if not m
      then return [post]
      else let post'' = subst post' (getAmounts post)
           in  do -- writeLog □ show post''
                  return □ case when of
                            Before → [post'', post]
                            After → [post, post'']
  case res of
    [] → return [post]
    r → return □ nub □ concat r

doRecord ∷ Dated Record → LState ()
doRecord rec = do
  setCurrentRecord rec
  doRecord' rec

doRecord' ∷ Dated Record → LState ()
doRecord' (At dt (PR post)) = do
  modify (setDate dt)
  post' ← checkTransaction post
  posts ← applyRules post'
  forM posts doTransaction
  putRecord (At dt (PR post'))
doRecord' rr@(At dt (RR ss)) = do
  modify (setDate dt)
  setRate ss
  putRecord rr
doRecord' (At dt (VR name amount)) = do
  acc ← getAccount name
  rs ← gets rates
  accs ← gets accounts
  let was = sumAccount acc
      delta = amountPlus rs amount □ negateAmount was
  if getValue delta > 0.0
    then do
           inc ← getIncFrom name
           let post = At dt □ PR □ Transaction 'A' "Correct balances" [name :<+ (F delta), Auto inc]
           doRecord post
    else if getValue delta ≡ 0.0
           then return ()
           else do
                  dec ← getDecTo name
                  let post = At dt □ PR □ Transaction 'A' "Correct balances" [name :<+ (F delta), Auto dec]
                  doRecord post
doRecord' (At _ (TR tpl)) = do
  st ← get
  let ts = templates st
      m = M.insert (tName tpl) tpl ts
  put □ st {templates = m}
doRecord' (At dt (CTR name args)) = do
  tpl ← getTemplate name
  let post = subst (tBody tpl) args
--   writeLog (show args)
--   writeLog (show post)
  doRecord □ At dt (PR post)
doRecord' (At _ (RuledP when rule post)) = do
  st ← get
  let rl = ruled st
  put □ st {ruled = rl ⧺ [(when,rule,post)]}
doRecord' (At _ (RuledC when rule name args)) = do
  tpl ← getTemplate name
  let post = subst (tBody tpl) args
  st ← get
  let rl = ruled st
  put □ st {ruled = rl ⧺ [(when,rule,post)]}
doRecord' (At _ (Hold name hld)) = do
  acc ← getAccount name
  st ← get
  let accs = accounts st
      acc' = acc {hold = hld}
      m = T.changeLeaf accs (T.mkPath name) acc'
  put □ st {accounts = m}

doRecords ∷ Conditions → [Dated Record] → LState ()
doRecords pred lst = forM_ (allToList pred lst) □ \r → do
    writeLog (show □ getDate r)
    doRecord r

