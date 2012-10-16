{-# LANGUAGE GADTs, FlexibleContexts #-}

module YaLedger.Reports.Common where

import Control.Applicative ((<$>))
import Control.Monad.Exception
import Control.Monad.Loc
import Data.Maybe
import Data.List
import Data.Decimal
import Data.Dates

import YaLedger.Types
import YaLedger.Output.Tables
import YaLedger.Output.ASCII
import YaLedger.Output.Pretty
import YaLedger.Monad
import YaLedger.Kernel
import YaLedger.Exceptions

isNotZero :: Amount -> Bool
isNotZero (x :# _) = x /= 0

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
      first = if head dates > start
                then [(start, head dates)]
                else []
  in  first ++ zip dates (tail dates)
      
splitQuery :: DateTime -> DateTime -> Query -> DateInterval -> [Query]
splitQuery first now qry int =
  let start = fromMaybe first (qStart qry)
      end   = fromMaybe now   (qEnd   qry)
      pairs = intervalsBetween start end int
      attrs = qAttributes qry
      adm   = qAllAdmin qry
  in  map (\(s, e) -> Query (Just s) (Just e) adm attrs) pairs

trimPath :: Maybe Int -> Path -> String
trimPath Nothing p = intercalate "/" p
trimPath (Just n) ps =
  let rlist = reverse ps
  in  case takeWhile ((< n) . snd) $ zip rlist $ tail $ scanl (+) 0 $ map length rlist of
        [] -> head ps
        xs -> intercalate "/" (reverse $ map fst xs)

showPostingAccount :: Maybe Int -> ChartOfAccounts -> Posting v t -> String
showPostingAccount t coa (CPosting acc _) = maybe "" (trimPath t) $ accountFullPath (getID acc) coa
showPostingAccount t coa (DPosting acc _) = maybe "" (trimPath t) $ accountFullPath (getID acc) coa

showPostingValueD :: Posting Decimal t -> String
showPostingValueD (CPosting acc x) = show (x :# getCurrency acc)
showPostingValueD (DPosting acc x) = show (x :# getCurrency acc)

showPostingValue :: Posting Amount t -> String
showPostingValue (CPosting _ x) = show x
showPostingValue (DPosting _ x) = show x

showE :: Ext (Entry Decimal Checked) -> Row
showE (Ext {getDate = date, getContent = (CEntry dt cr rd)}) =
    [prettyPrint date: replicate (m-1) "",
     map posting cr, map posting dt, rdS]
  where
    m = max (length cr) (length dt)
    rdS
      | rd == OneCurrency = []
      | otherwise = [show rd]

showE' :: Maybe Int -> ChartOfAccounts -> Ext (Entry Decimal Checked) -> Row
showE' t coa (Ext {getDate = date, getContent = (CEntry dt cr rd)}) =
    [prettyPrint date: replicate (m-1) "",
     map (showPostingAccount t coa) cr, map showPostingValueD cr,
     map (showPostingAccount t coa) dt, map showPostingValueD dt,
     rdS]
  where
    m = max (length cr) (length dt)
    rdS
      | rd == OneCurrency = []
      | otherwise = [show rd]

showB :: Currency -> Ext (Balance Checked) -> Row
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
       padding ++ [show bd ++ show currency]]

showB' :: Maybe Int -> ChartOfAccounts -> Currency -> Ext (Balance Checked) -> Row
showB' t coa currency (Ext {getDate = date, getContent = balance}) =
  let bd = balanceValue balance
      dt :: [Posting Decimal Debit]
      cr :: [Posting Decimal Credit]
      (dt, cr) = case causedBy balance of
                   Nothing -> ([], [])
                   Just (CEntry dt cr _) -> (dt, cr)
      m = max (length cr) (length dt)
      padding = replicate (m-1) ""
  in  [prettyPrint date: padding,
       map (showPostingAccount t coa) cr, map showPostingValueD cr,
       map (showPostingAccount t coa) dt, map showPostingValueD dt,
       padding ++ [show bd ++ show currency]]

posting :: Posting Decimal t -> String
posting (DPosting acc x) = getName acc ++ ": " ++ show (x :# getCurrency acc)
posting (CPosting acc x) = getName acc ++ ": " ++ show (x :# getCurrency acc)

showEntries :: (TableFormat a) => a -> Amount -> [Ext (Entry Decimal Checked)] -> String
showEntries fmt totals list =
  let l = map showE list
      footer = ["    TOTALS: " ++ show totals]
  in  unlines $
      tableGrid fmt [(ALeft,  ["DATE"]),
                     (ARight, ["CREDIT"]),
                     (ARight, ["DEBIT"]),
                     (ARight, ["RATES DIFF."])] l ++ footer

showEntries' :: (TableFormat a) => ChartOfAccounts -> a -> Amount -> [Ext (Entry Decimal Checked)] -> String
showEntries' coa fmt totals list =
  let l = map (showE' (maxFieldWidth fmt) coa) list
      footer = ["    TOTALS: " ++ show totals]
  in  unlines $
      tableGrid fmt [(ALeft,  ["DATE"]),
                     (ALeft,  ["CREDIT ACCOUNT"]),
                     (ARight, ["CREDIT AMOUNT"]),
                     (ALeft,  ["DEBIT ACCOUNT"]),
                     (ARight, ["DEBIT AMOUNT"]),
                     (ARight, ["RATES DIFF."])] l ++ footer

showEntriesBalances :: (TableFormat a) => a -> Amount -> [Ext (Balance Checked)] -> String
showEntriesBalances fmt totals list =
  let l = map (showB $ getCurrency totals) list
      footer = ["    TOTALS: " ++ show totals]
  in  unlines $
      tableGrid fmt [(ALeft,  ["DATE"]),
                     (ARight, ["CREDIT"]),
                     (ARight, ["DEBIT"]),
                     (ARight, ["BALANCE B/D"])] l ++ footer

showEntriesBalances' :: (TableFormat a) => ChartOfAccounts -> a -> Amount -> [Ext (Balance Checked)] -> String
showEntriesBalances' coa fmt totals list =
  let l = map (showB' (maxFieldWidth fmt) coa (getCurrency totals)) list
      footer = ["    TOTALS: " ++ show totals]
  in  unlines $
      tableGrid fmt [(ALeft,  ["DATE"]),
                     (ALeft,  ["CREDIT ACCOUNT"]),
                     (ARight, ["CREDIT AMOUNT"]),
                     (ALeft,  ["DEBIT ACCOUNT"]),
                     (ARight, ["DEBIT AMOUNT"]),
                     (ARight, ["BALANCE B/D"])] l ++ footer

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

