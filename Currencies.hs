{-# LANGUAGE TypeOperators, TypeSynonymInstances, PatternGuards #-}

module Currencies where

import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe

import Dates
import Lists

type Currency = String

type Rates = M.Map (Currency,Currency) Double

showRates rates = unlines $ map showPair $ M.assocs rates
  where
    showPair ((c1,c2), x) = c1 ++ " → " ++ c2 ++ ":\t" ++ show x

data Amount = Double :# Currency
infixr 7 :#

instance Show Amount where
  show (x :# c) = show x ++ c

data Account = Account {
                accName :: String,
                accCurrency :: Currency,
                incFrom :: Maybe Account,
                decTo :: Maybe Account,
                history :: [(DateTime,Double)] }

instance Show Account where
  show (Account name curr _ _ hist) = name ++ ":" ++ curr ++ "\n" ++ strHist
    where
      strHist = unlines $ map (\(dt,x) -> show dt ++ ":\t" ++ show x) hist

data LedgerState = LS {
                     now :: DateTime,
                     accounts :: M.Map String Account,
                     records :: [Dated Record],
                     rates :: Rates }

instance Show LedgerState where
  show (LS now accs recs rates) = unlines $ [show now, showAccs, showRates rates] ++ map show recs
    where
      showAccs = unlines $ map (show.snd) $ M.assocs accs

type LState a = State LedgerState a

data Dated a = At {
                 getDate :: DateTime,
                 content :: a }

instance (Show a) => Show (Dated a) where
  show (At dt a) = "@" ++ show dt ++ "\n" ++ show a

data Posting = Posting {
                status :: Char,
                description :: String,
                parts :: [Part] }

instance Show Posting where
  show (Posting s d ps) = unlines $ (s:' ':d):map show ps

data Part = String :<+ Amount
          | Auto Account
infixl 6 :<+

instance Show Part where
  show (s :<+ am) = s ++ " :<+ " ++ show am
  show (Auto acc) = accName acc ++ " :<+ ???"

data RegularPosting = RegularPosting DateTime DateInterval Posting
  deriving (Show)

data Record = PR Posting
            | RR SetRate
            | VR String Amount
            | RegR RegularPosting 

instance Show Record where
  show (PR post) = show post
  show (RR rr) = show rr
  show (VR acc amount) = acc ++ " == " ++ show amount
  show (RegR rp) = show rp

data SetRate = Currency := Amount
infixl 6 :=

instance Show SetRate where
  show (c := am) = c ++ " := " ++ show am

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

putAmount :: String -> Amount -> LState ()
putAmount name (x :# c) = do
  accs <- gets accounts
  rs <- gets rates
  dt <- gets now
  acc <- getAccount name
  recs <- gets records
  let y = convert rs x c (accCurrency acc)
      acc' = acc {history = (history acc) ++ [(dt,y)]}
      m = M.insert name acc' accs
  put $ LS dt m recs rs

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
getAmount (_ :<+ a) = a

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

fixParts :: Rates -> [Part] -> Part -> [Part]
fixParts rates parts (Auto acc) = parts ++ [accName acc :<+ amount]
  where
    amount = negateAmount $ sumAmounts rates $ map getAmount parts

checkParts :: Rates -> [Part] -> LState [Part]
checkParts rates parts = 
  if (getValue $ sumAmounts rates $ map getAmount parts) == 0.0
    then return parts
    else fail "Posting does not balance"

sumAccount :: Account -> Amount
sumAccount acc = (sum $ map snd $ history acc) :# (accCurrency acc)

doPart :: Part -> LState ()
doPart (acc :<+ amount) = putAmount acc amount

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

doRecord :: Dated Record -> LState ()
doRecord rr@(At dt (PR post)) = do
  modify (setDate dt)
  doPosting post
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
           let post = At dt $ PR $ Posting 'A' "Autogenerated posting" [name :<+ delta, Auto inc]
           doRecord post
    else if getValue delta == 0.0
           then return ()
           else do
                  dec <- getDecTo name
                  let post = At dt $ PR $ Posting 'A' "Autogenerated posting" [name :<+ delta, Auto dec]
                  doRecord post

doRecords :: DateTime -> [Dated Record] -> LState ()
doRecords dt lst = forM_ (allToList ((< dt).getDate) lst) doRecord

