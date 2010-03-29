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

writeLog :: String -> LState ()
writeLog msg = do
  st <- get
  let was = messages st
  put $ st { messages = msg: was }

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

setRate :: SetRate -> LState ()
setRate (c1 := (x :# c2)) = do
  st <- get
  let rs = rates st
      m = M.insert (c1,c2) x rs
  put $ st {rates = m}

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

amountGT :: Amount -> Amount -> LState Bool
amountGT a1 a2 = do
  c <- compareAmounts a1 a2
  return $ c == GT

amountLT :: Amount -> Amount -> LState Bool
amountLT a1 a2 = do
  c <- compareAmounts a1 a2
  return $ c == LT

