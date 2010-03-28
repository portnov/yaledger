{-# LANGUAGE TypeOperators, TypeSynonymInstances, PatternGuards, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Currencies where

import Control.Monad
import Control.Monad.State.Class
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.List.Utils (split)
import Data.Generics hiding (GT)
import Text.Regex.PCRE hiding (match)

import Types
import Dates
import Lists
import qualified Tree as T

mkPath :: String -> [String]
mkPath str = split "/" str

sumAccountsTree :: Rates-> T.Tree Currency Account-> T.Tree Amount Account
sumAccountsTree rs tree = T.partFold foldA plus foldS tree
  where
    foldA :: Currency -> [Account] -> Amount
    foldA c accs = convertAmount rs c $ sumAmounts rs $ map sumAccount accs
    plus :: Amount -> Amount -> Amount
    plus a1 a2 = amountPlus rs a1 a2
    foldS :: [Amount] -> Amount
    foldS = sumAmounts rs

writeLog :: String -> LState ()
writeLog msg = do
  st <- get
  let was = messages st
  put $ st { messages = msg: was }

datedSeq :: Dated a -> DateInterval -> [Dated a]
datedSeq (At dt x) int = [At d x | d <- datesFromEvery dt int]

regularToList :: RegularTransaction -> [Dated Record]
regularToList (RegularTransaction date int post) = datedSeq (At date $ PR post) int

allToList :: (Dated Record -> Bool) -> [Dated Record] -> [Dated Record]
allToList pred recs = takeWhile pred $ mergeOn getDate (ones : map toList regs)
  where
    isReg (At _ (RegR _)) = True
    isReg _               = False
    regs :: [Dated Record]
    regs = filter isReg recs
    ones :: [Dated Record]
    ones = filter (not.isReg) recs
    toList :: Dated Record -> [Dated Record]
    toList (At _ (RegR reg)) = regularToList reg
    toList x = [x]

setDate :: DateTime -> LedgerState -> LedgerState
setDate dt ls = ls {now = dt}

getRate :: Rates -> Currency -> Currency -> Double
getRate rates c1 c2 = fromMaybe 1.0 $ M.lookup (c1,c2) rates

convert :: Rates -> Double -> Currency -> Currency -> Double
convert rates x c1 c2 | c1 == c2  = x
                      | otherwise = x*getRate rates c1 c2

convertAmount :: Rates -> Currency -> Amount -> Amount
convertAmount rs c2 (a:#c1) = (convert rs a c1 c2):#c2

compareAmounts :: Amount -> Amount -> LState Ordering
compareAmounts a1 a2 = do
  rs <- gets rates
  let x = getValue a1
      y = convert rs  (getValue a2) (getCurrency a2) (getCurrency a1)
  return $ compare x y

putAmount :: String -> Amount -> LState ()
putAmount name (x :# c) = do
  st <- get
  let accs = accounts st
      rs   = rates st
      dt   = now st
  acc <- getAccount name
  let y = convert rs x c (accCurrency acc)
      acc' = acc {history = (history acc) ++ [(dt,y)]}
      m = T.changeLeaf accs (mkPath name) acc'
  put $ st {accounts = m}

amountPlus :: Rates -> Amount -> Amount -> Amount
amountPlus rates (x :# c1) (y :# c2) = (x + convert rates y c2 c1) :# c1

sumAmounts :: Rates -> [Amount] -> Amount
sumAmounts _ [] = 0 :# ""
sumAmounts rates amounts = foldl (amountPlus rates) (0 :# firstCur) amounts
  where
    firstCur = getCurrency (head amounts)

negateAmount :: Amount -> Amount
negateAmount (x :# c) = (-x) :# c

getCurrency :: Amount -> Currency
getCurrency (_ :# c) = c

getValue :: Amount -> Double
getValue (x :# _) = x

getAmount :: Posting -> Amount
getAmount (_ :<+ a) = defAmount a

isAuto :: Posting -> Bool
isAuto (Auto _) = True
isAuto _        = False

getAccount :: String -> LState Account
getAccount name = do
  accs <- gets accounts
  case T.lookup (mkPath name) accs of
    Nothing -> fail $ "Unknown account: " ++ name
    Just acc -> return acc

getIncFrom :: String -> LState Account
getIncFrom name = do
  acc <- getAccount name
  case incFrom acc of
    NoLink -> fail $ "income account for " ++ name ++ " is not set"
    LinkTo acc' -> return acc'

getDecTo :: String -> LState Account
getDecTo name = do
  acc <- getAccount name
  case decTo acc of
    NoLink -> fail $ "outcome account for " ++ name ++ " is not set"
    LinkTo acc' -> return acc'

getTemplate :: String -> LState Template
getTemplate name = do
  tpls <- gets templates
  case M.lookup name tpls of
    Nothing -> fail $ "Unknown template: " ++ name
    Just tpl -> return tpl

fixPostings :: Rates -> [Posting] -> Posting -> [Posting]
fixPostings rates parts (Auto acc) = parts ++ [acc :<+ (F amount)]
  where
    amount = negateAmount $ sumAmounts rates $ map getAmount parts

checkPostings :: Rates -> [Posting] -> LState [Posting]
checkPostings rates parts = 
  if (getValue $ sumAmounts rates $ map getAmount parts) == 0.0
    then return parts
    else fail "Transaction does not balance"

doPosting :: Posting -> LState ()
doPosting (acc :<+ amount) = putAmount acc (defAmount amount)

checkTransaction :: Transaction -> LState Transaction
checkTransaction post = do
  let srcPostings = parts post
  rs <- gets rates
  parts' <- forM srcPostings $ \p -> do
              case p of
                name :<+ (F (x :# "%")) -> do
                  acc <- getAccount name
                  let (s :# c) = sumAccount acc
                      y = s*x/100.0
                  return $ name :<+ (F (y :# c))
                p' -> return p'
  parts'' <- case filter isAuto parts' of
              [p] -> return $ fixPostings rs (filter (not . isAuto) parts') p
              []  -> checkPostings rs parts'
              _   -> fail "More than one lines without amount given"
  return $ post {parts = parts''}

doTransaction :: Transaction -> LState ()
doTransaction post = do
  post' <- checkTransaction post
  forM_ (parts post') doPosting

setRate :: SetRate -> LState ()
setRate (c1 := (x :# c2)) = do
  st <- get
  let rs = rates st
      m = M.insert (c1,c2) x rs
  put $ st {rates = m}

putRecord :: Dated Record -> LState ()
putRecord rr | At dt (PR post) <- rr = do
    post' <- checkTransaction post
    modify (add $ At dt (PR post'))
             | otherwise = modify (add rr)
  where
    add r st = st {records = records st ++ [r]}

orM :: (Monad m) => [m Bool] -> m Bool
orM = (liftM or) . sequence

getPercents (x :# c) p = (x*p/100.0) :# c

subst :: Transaction -> [Amount] -> Transaction
subst tpl args = everywhere (mkT subst') tpl
  where
    subst' (F x) = F x
    subst' (P p i def) = 
      if i >= length args
        then F def
        else F $ getPercents (args !! i) p

getAccounts :: Transaction -> [(String,Amount)]
getAccounts post = map convert $ parts post
  where
    convert (acc :<+ a) = (acc, defAmount a)
    convert (Auto a) = error $ "Internal error: unexpected Auto posting-part: " ++ a

getAmounts :: Transaction -> [Amount]
getAmounts post = concatMap get (parts post)
  where
    get (_ :<+ a) = [defAmount a]
    get (Auto _)  = []

amountGT :: Amount -> Amount -> LState Bool
amountGT a1 a2 = do
  c <- compareAmounts a1 a2
  return $ c == GT

amountLT :: Amount -> Amount -> LState Bool
amountLT a1 a2 = do
  c <- compareAmounts a1 a2
  return $ c == LT

matchName :: String -> (String,Amount) -> Bool
matchName regex (name,_) = name =~ regex

match :: Rule -> Transaction -> LState Bool
match (DescrMatch regex) post = return $ (description post) =~ regex
match (regex :> amount) post = orM $ map match' $ filter (matchName regex) $ getAccounts post
  where
    match' (acc,amount') = amountGT amount' amount
match (regex :< amount) post = orM $ map match' $ filter (matchName regex) $ getAccounts post
  where
    match' (acc,amount') = amountLT amount' amount

applyRules :: Transaction -> LState [Transaction]
applyRules post = do
  rls <- gets ruled
  res <- forM rls $ \(when,rule,post') -> do
    m <- match rule post
    if not m
      then return [post]
      else let post'' = subst post' (getAmounts post)
           in  do writeLog $ show post''
                  return $ case when of
                            Before -> [post'', post]
                            After -> [post, post'']
  case res of
    [] -> return [post]
    r -> return $ nub $ concat r

doRecord :: Dated Record -> LState ()
doRecord (At dt (PR post)) = do
  modify (setDate dt)
  post' <- checkTransaction post
  posts <- applyRules post'
  forM posts doTransaction
  putRecord (At dt (PR post'))
doRecord rr@(At dt (RR ss)) = do
  modify (setDate dt)
  setRate ss
  putRecord rr
doRecord (At dt (VR name amount)) = do
  acc <- getAccount name
  rs <- gets rates
  let was = sumAccount acc
      delta = amountPlus rs amount $ negateAmount was
  if getValue delta > 0.0
    then do
           inc <- getIncFrom name
           let post = At dt $ PR $ Transaction 'A' "Correct balances" [name :<+ (F delta), Auto $ accName inc]
           doRecord post
    else if getValue delta == 0.0
           then return ()
           else do
                  dec <- getDecTo name
                  let post = At dt $ PR $ Transaction 'A' "Correct balances" [name :<+ (F delta), Auto $ accName dec]
                  doRecord post
doRecord (At _ (TR tpl)) = do
  st <- get
  let ts = templates st
      m = M.insert (tName tpl) tpl ts
  put $ st {templates = m}
doRecord (At dt (CTR name args)) = do
  tpl <- getTemplate name
  let post = subst (tBody tpl) args
  doRecord $ At dt (PR post)
doRecord (At _ (RuledP when rule post)) = do
  st <- get
  let rl = ruled st
  put $ st {ruled = rl ++ [(when,rule,post)]}
doRecord (At _ (RuledC when rule name args)) = do
  tpl <- getTemplate name
  let post = subst (tBody tpl) args
  st <- get
  let rl = ruled st
  put $ st {ruled = rl ++ [(when,rule,post)]}

doRecords :: DateTime -> [Dated Record] -> LState ()
doRecords dt lst = forM_ (allToList ((< dt).getDate) lst) doRecord

