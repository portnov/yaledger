{-# LANGUAGE TypeOperators, TypeSynonymInstances, PatternGuards, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Currencies where

import Control.Monad
import Control.Monad.State.Class
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Generics hiding (GT)
import Text.Regex.PCRE hiding (match)

import Types
import Dates
import Lists

datedSeq :: Dated a -> DateInterval -> [Dated a]
datedSeq (At dt x) int = [At d x | d <- datesFromEvery dt int]

regularToList :: RegularPosting -> [Dated Record]
regularToList (RegularPosting date int post) = datedSeq (At date $ PR post) int

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
      m = M.insert name acc' accs
  put $ st {accounts = m}

amountPlus :: Rates -> Amount -> Amount -> Amount
amountPlus rates (x :# c1) (y :# c2) = (x + convert rates y c2 c1) :# c1

sumAmounts :: Rates -> [Amount] -> Amount
sumAmounts rates amounts = foldl (amountPlus rates) (0 :# firstCur) amounts
  where
    firstCur = getCurrency (head amounts)

negateAmount :: Amount -> Amount
negateAmount (x :# c) = (-x) :# c

getCurrency :: Amount -> Currency
getCurrency (_ :# c) = c

getValue :: Amount -> Double
getValue (x :# _) = x

getAmount :: Part -> Amount
getAmount (_ :<+ a) = defAmount a

isAuto :: Part -> Bool
isAuto (Auto _) = True
isAuto _        = False

getAccount :: String -> LState Account
getAccount name = do
  accs <- gets accounts
  case M.lookup name accs of
    Nothing -> fail $ "Unknown account: " ++ name
    Just acc -> return acc

getIncFrom :: String -> LState Account
getIncFrom name = do
  acc <- getAccount name
  case incFrom acc of
    Nothing -> fail $ "income account for " ++ name ++ " is not set"
    Just acc' -> return acc'

getDecTo :: String -> LState Account
getDecTo name = do
  acc <- getAccount name
  case decTo acc of
    Nothing -> fail $ "outcome account for " ++ name ++ " is not set"
    Just acc' -> return acc'

getTemplate :: String -> LState Template
getTemplate name = do
  tpls <- gets templates
  case M.lookup name tpls of
    Nothing -> fail $ "Unknown template: " ++ name
    Just tpl -> return tpl

fixParts :: Rates -> [Part] -> Part -> [Part]
fixParts rates parts (Auto acc) = parts ++ [accName acc :<+ (F amount)]
  where
    amount = negateAmount $ sumAmounts rates $ map getAmount parts

checkParts :: Rates -> [Part] -> LState [Part]
checkParts rates parts = 
  if (getValue $ sumAmounts rates $ map getAmount parts) == 0.0
    then return parts
    else fail "Posting does not balance"

doPart :: Part -> LState ()
doPart (acc :<+ amount) = putAmount acc (defAmount amount)

checkPosting :: Posting -> LState Posting
checkPosting post = do
  let srcParts = parts post
  rs <- gets rates
  parts' <- forM srcParts $ \p -> do
              case p of
                name :<+ (F (x :# "%")) -> do
                  acc <- getAccount name
                  let (s :# c) = sumAccount acc
                      y = s*x/100.0
                  return $ name :<+ (F (y :# c))
                p' -> return p'
  parts'' <- case filter isAuto parts' of
              [p] -> return $ fixParts rs (filter (not . isAuto) parts') p
              []  -> checkParts rs parts'
              _   -> fail "More than one lines without amount given"
  return $ post {parts = parts''}

doPosting :: Posting -> LState ()
doPosting post = do
  post' <- checkPosting post
  forM_ (parts post') doPart

setRate :: SetRate -> LState ()
setRate (c1 := (x :# c2)) = do
  st <- get
  let rs = rates st
      m = M.insert (c1,c2) x rs
  put $ st {rates = m}

putRecord :: Dated Record -> LState ()
putRecord rr | At dt (PR post) <- rr = do
    post' <- checkPosting post
    modify (add $ At dt (PR post'))
             | otherwise = modify (add rr)
  where
    add r st = st {records = records st ++ [r]}

orM :: (Monad m) => [m Bool] -> m Bool
orM = (liftM or) . sequence

getPercents (x :# c) p = (x*p/100.0) :# c

subst :: Posting -> [Amount] -> Posting
subst tpl args = everywhere (mkT subst') tpl
  where
    subst' (F x) = F x
    subst' (P p i def) = 
      if i >= length args
        then F def
        else F $ getPercents (args !! i) p

getAccounts :: Posting -> [(String,Amount)]
getAccounts post = map convert $ parts post
  where
    convert (acc :<+ a) = (acc, defAmount a)
    convert (Auto a) = error $ "Internal error: unexpected Auto posting-part: " ++ show a

getAmounts :: Posting -> [Amount]
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

match :: Rule -> Posting -> LState Bool
match (DescrMatch regex) post = return $ (description post) =~ regex
match (regex :> amount) post = orM $ map match' $ filter (matchName regex) $ getAccounts post
  where
    match' (acc,amount') = amountGT amount' amount
match (regex :< amount) post = orM $ map match' $ filter (matchName regex) $ getAccounts post
  where
    match' (acc,amount') = amountLT amount' amount

applyRules :: Posting -> LState [Posting]
applyRules post = do
  rls <- gets ruled
  res <- forM rls $ \(when,rule,post') -> do
    m <- match rule post
    if not m
      then return [post]
      else let post'' = subst post' (getAmounts post)
           in  return $ case when of
                          Before -> [post'', post]
                          After -> [post, post'']
  case res of
    [] -> return [post]
    r -> return $ nub $ concat r

doRecord :: Dated Record -> LState ()
doRecord (At dt (PR post)) = do
  modify (setDate dt)
  post' <- checkPosting post
  posts <- applyRules post'
  forM posts doPosting
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
           let post = At dt $ PR $ Posting 'A' "Autogenerated posting" [name :<+ (F delta), Auto inc]
           doRecord post
    else if getValue delta == 0.0
           then return ()
           else do
                  dec <- getDecTo name
                  let post = At dt $ PR $ Posting 'A' "Autogenerated posting" [name :<+ (F delta), Auto dec]
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

