{-# LANGUAGE GADTs, FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.Common where

import Control.Applicative ((<$>))
import Control.Monad.Exception
import Control.Monad.Loc
import Data.Maybe
import Data.Decimal
import Data.Dates

import YaLedger.Types
import YaLedger.Strings
import YaLedger.Pretty
import YaLedger.Monad
import YaLedger.Exceptions
import YaLedger.Logger

isNotZero :: Amount -> Bool
isNotZero (x :# _) = x /= 0

negateInterval :: DateInterval -> DateInterval
negateInterval (Days n)   = Days (negate n)
negateInterval (Weeks n)  = Weeks (negate n)
negateInterval (Months n) = Months (negate n)
negateInterval (Years n)  = Years (negate n)

minusInterval :: DateTime -> DateInterval -> DateTime
minusInterval date int = date `addInterval` negateInterval int

datesBackFrom :: DateTime -> DateInterval -> [DateTime]
datesBackFrom date int = go date
  where
    go dt = dt: go (dt `minusInterval` int)

datesBetween :: DateTime -> DateTime -> DateInterval -> [DateTime]
datesBetween start end int =
  reverse $ takeWhile (>= start) $ datesBackFrom end int

intervalsBetween :: DateTime -> DateTime -> DateInterval -> [(DateTime, DateTime)]
intervalsBetween start end int =
  let dates = datesBetween start end int
  in  zip dates (tail dates)

splitQuery :: DateTime -> DateTime -> Query -> DateInterval -> [Query]
splitQuery first now qry int =
  let start = fromMaybe first (qStart qry)
      end   = fromMaybe now   (qEnd   qry)
      pairs = intervalsBetween start end int
      attrs = qAttributes qry
      adm   = qAllAdmin qry
  in  map (\(s, e) -> Query (Just s) (Just e) adm attrs) pairs

showE :: Ext (Entry Decimal Checked) -> [[String]]
showE (Ext {getDate = date, getContent = (CEntry dt cr rd)}) =
    [prettyPrint date: replicate (m-1) "",
     map posting cr, map posting dt, rdS]
  where
    m = max (length cr) (length dt)
    rdS
      | rd == OneCurrency = []
      | otherwise = [show rd]

showB :: Currency -> Ext (Balance Checked) -> [[String]]
showB currency (Ext {getDate = date, getContent = balance}) =
  let bd = balanceValue balance
      dt :: [Posting Decimal Debit]
      cr :: [Posting Decimal Credit]
      (dt, cr) = case causedBy balance of
                   Nothing -> ([], [])
                   Just (CEntry dt cr _) -> (dt, cr)
      m = max (length cr) (length dt)
      padding = replicate (m-1) ""
  in  [prettyPrint date: padding,
       map posting cr, map posting dt,
       padding ++ [show bd ++ currency]]

posting :: Posting Decimal t -> String
posting (DPosting acc x) = getName acc ++ ": " ++ show (roundTo 4 x) ++ getCurrency acc
posting (CPosting acc x) = getName acc ++ ": " ++ show (roundTo 4 x) ++ getCurrency acc

showEntries :: Amount -> [Ext (Entry Decimal Checked)] -> String
showEntries totals list =
  let l = map showE list
      footer = ["    TOTALS: " ++ show totals]
  in  unlines $
          grid ["DATE", "CREDIT", "DEBIT", "RATES DIFF."] l ++ footer

showEntriesBalances :: Amount -> [Ext (Balance Checked)] -> String
showEntriesBalances totals list =
  let l = map (showB $ getCurrency totals) list
      footer = ["    TOTALS: " ++ show totals]
  in  unlines $
          grid ["DATE", "CREDIT", "DEBIT", "BALANCE B/D"] l ++ footer

causedByExt :: Ext (Balance Checked) -> Maybe (Ext (Entry Decimal Checked))
causedByExt (Ext date pos attrs p) =
  Ext date pos attrs <$> causedBy p

getEntries :: (Throws InternalError l,
               HasBalances a)
           => a
           -> Ledger l [Ext (Entry Decimal Checked)]
getEntries acc = do
  balances <- readIOList (accountBalances acc)
  return $ mapMaybe causedByExt balances

