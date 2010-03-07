{-# LANGUAGE TypeOperators, TypeSynonymInstances, PatternGuards, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Currencies where

import Control.Monad
import Control.Monad.State.Class
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Generics hiding (GT)
import Text.Regex.PCRE hiding (match)

import Dates
import Lists

type Currency = String

type Rates = M.Map (Currency,Currency) Double

showRates rates = unlines $ map showPair $ M.assocs rates
  where
    showPair ((c1,c2), x) = c1 ++ " → " ++ c2 ++ ":\t" ++ show x

data Amount = Double :# Currency
  deriving (Eq,Data,Typeable)
infixr 7 :#

instance Show Amount where
  show (x :# c) = show x ++ c

data Account = Account {
                accName :: String,
                accCurrency :: Currency,
                incFrom :: Maybe Account,
                decTo :: Maybe Account,
                history :: [(DateTime,Double)] }
  deriving (Eq,Data,Typeable)

instance Show Account where
  show (Account name curr _ _ hist) = name ++ ":" ++ curr ++ "\n" ++ strHist
    where
      strHist = unlines $ map (\(dt,x) -> show dt ++ ":\t" ++ show x) hist

data LedgerState = LS {
                     now :: DateTime,
                     accounts :: M.Map String Account,
                     records :: [Dated Record],
                     rates :: Rates,
                     templates :: M.Map String Template,
                     ruled :: [(RuleWhen, Rule, Posting)] }

instance Show LedgerState where
  show st@(LS now accs recs rates _ _) = unlines $ [show now, showAccs, showRates rates]
                                              ++ ["Balances:\n" ++ showPairs (balances st)]
                                              ++ map show recs
    where
      showAccs = unlines $ map (show.snd) $ M.assocs accs

data LError = LError {
                eRecord :: Maybe Record,
                eState :: LedgerState,
                eReason :: String }

instance Show LError where
  show (LError rec st reason) = "Error: " ++ reason ++ " (at " ++ show rec ++ ")\n" ++ show st

newtype AState s a = AState { runState :: s -> Either LError (a, s) }
type LState a = AState LedgerState a

instance Monad (AState LedgerState) where
  return a = AState $ \s -> Right (a,s)
  m >>= k  = AState $ \s -> 
               case runState m s of
                 Right (a,s') -> runState (k a) s'
                 Left err     -> Left err
  fail str = AState $ \s -> Left (LError Nothing s str)

instance MonadState LedgerState (AState LedgerState) where
    get   = AState $ \s -> Right (s, s)
    put s = AState $ \_ -> Right ((), s)

data Dated a = At {
                 getDate :: DateTime,
                 content :: a }
  deriving (Data,Typeable)

instance (Show a) => Show (Dated a) where
  show (At dt a) = "@" ++ show dt ++ "\n" ++ show a

data Posting = Posting {
                status :: Char,
                description :: String,
                parts :: [Part] }
  deriving (Eq,Data,Typeable)

instance Show Posting where
  show (Posting s d ps) = unlines $ (s:' ':d):map show ps

data AmountParam = F Amount
                 | Int :? Amount
  deriving (Eq, Data,Typeable)
infixl 5 :?

instance Show AmountParam where
  show (F amount) = show amount
  show (n :? def) = '#':show n ++ "?=" ++ show def

defAmount :: AmountParam -> Amount
defAmount (F amount) = amount
defAmount (_ :? amount) = amount

data Part = String :<+ AmountParam
          | Auto Account
  deriving (Eq, Data,Typeable)
infixl 6 :<+

instance Show Part where
  show (s :<+ am) = s ++ " :<+ " ++ show am
  show (Auto acc) = accName acc ++ " :<+ ???"

data RegularPosting = RegularPosting DateTime DateInterval Posting
  deriving (Show,Data,Typeable)

data Template = Template {
                  tName :: String,
                  tNParams :: Int,
                  tBody :: Posting }
  deriving (Data,Typeable)

instance Show Template where
  show (Template name n post) = name ++ "/" ++ show n ++ ":\n" ++ show post

data Record = PR Posting
            | RR SetRate
            | VR String Amount
            | RegR RegularPosting 
            | TR Template
            | CTR String [Amount]
            | RuledP RuleWhen Rule Posting
            | RuledC RuleWhen Rule String [Amount]
  deriving (Data,Typeable)

instance Show Record where
  show (PR post) = show post
  show (RR rr) = show rr
  show (VR acc amount) = acc ++ " == " ++ show amount
  show (RegR rp) = show rp
  show (TR tp) = show tp
  show (CTR name args) = name ++ "(" ++ (intercalate ", " $ map show args) ++ ")"
  show (RuledP when rule post) = show when ++ " " ++ show rule ++ " -->\n" ++ show post
  show (RuledC when rule name args) = show when ++ " " ++ show rule ++ " --> " ++ name ++ "(" ++ (intercalate ", " $ map show args) ++ ")"

data SetRate = Currency := Amount
  deriving (Data,Typeable)
infixl 6 :=

instance Show SetRate where
  show (c := am) = c ++ " := " ++ show am

data Rule = DescrMatch String
          | String :> Amount
          | String :< Amount
  deriving (Show,Data,Typeable)

data RuleWhen = Before | After
  deriving (Show,Data,Typeable)

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
      y = convert rs (getValue a2) (getCurrency a2) (getCurrency a1)
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

sumAccount :: Account -> Amount
sumAccount acc = (sum $ map snd $ history acc) :# (accCurrency acc)

balances :: LedgerState -> [(String, Amount)]
balances st = [(name, sumAccount acc) | (name, acc) <- M.assocs $ accounts st]

showPairs :: (Show a, Show b) => [(a,b)] -> String
showPairs pairs = unlines $ map showPair pairs
  where
    showPair (a,b) = show a ++ "\t" ++ show b

doPart :: Part -> LState ()
doPart (acc :<+ amount) = putAmount acc (defAmount amount)

checkPosting :: Posting -> LState Posting
checkPosting post = do
  let srcParts = parts post
  rs <- gets rates
  parts' <- case filter isAuto srcParts of
              [p] -> return $ fixParts rs (filter (not . isAuto) srcParts) p
              []  -> checkParts rs srcParts
              _   -> fail "More than one lines without amount given"
  return $ post {parts = parts'}

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

subst :: Posting -> [Amount] -> Posting
subst tpl args = everywhere (mkT subst') tpl
  where
    subst' (F x) = F x
    subst' (i :? def) = 
      if i >= length args
        then F def
        else F (args !! i)

getAccounts :: Posting -> [(String,Amount)]
getAccounts post = map convert $ parts post
  where
    convert (acc :<+ a) = (acc, defAmount a)
    convert (Auto a) = error $ "Internal error: unexpected Auto posting-part: " ++ show a

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
      else return $ case when of
                      Before -> [post', post]
                      After -> [post, post']
  case res of
    [] -> return [post]
    r -> return $ nub $ concat r

doRecord :: Dated Record -> LState ()
doRecord rr@(At dt (PR post)) = do
  modify (setDate dt)
  post' <- checkPosting post
  posts <- applyRules post'
  forM posts doPosting
  putRecord rr
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

