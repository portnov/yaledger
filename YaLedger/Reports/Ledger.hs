{-# LANGUAGE GADTs, FlexibleContexts, MultiParamTypeClasses #-}

module YaLedger.Reports.Ledger where

import Control.Applicative ((<$>))
import Control.Monad.Exception
import Control.Monad.Loc
import Data.Maybe
import Data.List
import Data.Decimal
import Data.Dates

import YaLedger.Types
import YaLedger.Output
import YaLedger.Kernel
import YaLedger.Exceptions
import YaLedger.Reports.Common

-- | Show amount: show currency only if there is no CNoCurrencies flag in options.
showAmt :: [CommonFlags] -> Amount -> String
showAmt options a@(x :# c)
  | CNoCurrencies `elem` options = prettyPrint x
  | otherwise = show a

-- | Show BalanceInfo: show currency only if there is no CNoCurrencies flag in options.
showBI :: [CommonFlags] -> BalanceInfo Amount -> String
showBI _ (BalanceInfo Nothing Nothing) = "NA"
showBI options (BalanceInfo (Just x) Nothing) = showAmt options x
showBI options (BalanceInfo Nothing (Just x)) = showAmt options x
showBI options (BalanceInfo (Just a) (Just l))
    | a == l = showAmt options a
    | otherwise = showAmt options a ++ " / " ++ showAmt options l

showPostingAccount :: Maybe Int -> ChartOfAccounts -> Posting v t -> String
showPostingAccount t coa (CPosting acc _ _) = maybe "" (trimPath t) $ accountFullPath (getID acc) coa
showPostingAccount t coa (DPosting acc _ _) = maybe "" (trimPath t) $ accountFullPath (getID acc) coa

showPostingValueD :: Bool -> Posting Decimal t -> String
showPostingValueD True (CPosting acc x _) = show (x :# getCurrency acc)
showPostingValueD True (DPosting acc x _) = show (x :# getCurrency acc)
showPostingValueD False (CPosting _ x _) = show x
showPostingValueD False (DPosting _ x _) = show x

showPostingValue :: Posting Amount t -> String
showPostingValue (CPosting _ x _) = show x
showPostingValue (DPosting _ x _) = show x

showE :: Ext (Entry Decimal Checked) -> Row
showE (Ext {getDate = date, getContent = (CEntry dt cr rd)}) =
    [prettyPrint date: replicate (m-1) "",
     map posting dt, map posting cr, rdS]
  where
    m = max (length cr) (length dt)
    rdS
      | rd == OneCurrency = []
      | otherwise = [show rd]

showE' :: Maybe Int -> Bool -> ChartOfAccounts -> Ext (Entry Decimal Checked) -> Row
showE' t showCurrs coa (Ext {getDate = date, getContent = (CEntry dt cr rd)}) =
    [prettyPrint date: replicate (m-1) "",
     map (showPostingAccount t coa) dt, map (showPostingValueD showCurrs) dt,
     map (showPostingAccount t coa) cr, map (showPostingValueD showCurrs) cr,
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
       map posting dt, map posting cr,
       padding ++ [show bd ++ show currency]]

showB' :: Maybe Int -> Bool -> BalanceQuery -> ChartOfAccounts -> Currency -> Ext (Balance Checked) -> Row
showB' t showCurrs bqry coa currency (Ext {getDate = date, getContent = balance}) =
  let bi = getBalanceInfo bqry balance
      dt :: [Posting Decimal Debit]
      cr :: [Posting Decimal Credit]
      (dt, cr) = case causedBy balance of
                   Nothing -> ([], [])
                   Just (CEntry dt cr _) -> (dt, cr)
      m = max (length cr) (length dt)
      padding = replicate (m-1) ""
      mbShow c = if showCurrs then show c else ""
  in  [prettyPrint date: padding,
       map (showPostingAccount t coa) dt, map (showPostingValueD showCurrs) dt,
       map (showPostingAccount t coa) cr, map (showPostingValueD showCurrs) cr,
       padding ++ [show bi ++ mbShow currency]]

posting :: Posting Decimal t -> String
posting (DPosting acc x _) = getName acc ++ ": " ++ show (x :# getCurrency acc)
posting (CPosting acc x _) = getName acc ++ ": " ++ show (x :# getCurrency acc)

showEntries :: (TableFormat a) => a -> Amount -> [Ext (Entry Decimal Checked)] -> String
showEntries fmt totals list =
  let l = map showE list
      footer = showFooter fmt $ "    TOTALS: " ++ show totals
  in  unlines $
      tableGrid fmt [(ALeft,  ["DATE"]),
                     (ARight, ["DEBIT"]),
                     (ARight, ["CREDIT"]),
                     (ARight, ["RATES DIFF."])] l ++ footer

showEntries' :: (TableFormat a) => ChartOfAccounts -> a -> Amount -> Bool -> [Ext (Entry Decimal Checked)] -> String
showEntries' coa fmt totals showCurrs list =
  let l = map (showE' (maxFieldWidth fmt) showCurrs coa) list
      footer = showFooter fmt $ "    TOTALS: " ++ show totals
  in  unlines $
      tableGrid fmt [(ALeft,  ["DATE"]),
                     (ALeft,  ["DEBIT ACCOUNT"]),
                     (ARight, ["AMOUNT"]),
                     (ALeft,  ["CREDIT ACCOUNT"]),
                     (ARight, ["AMOUNT"]),
                     (ARight, ["RATES DIFF."])] l ++ footer

showEntriesBalances :: (TableFormat a) => a -> Amount -> [Ext (Balance Checked)] -> String
showEntriesBalances fmt totals list =
  let l = map (showB $ getCurrency totals) list
      footer = showFooter fmt $ "    TOTALS: " ++ show totals
  in  unlines $
      tableGrid fmt [(ALeft,  ["DATE"]),
                     (ARight, ["DEBIT"]),
                     (ARight, ["CREDIT"]),
                     (ARight, ["BALANCE B/D"])] l ++ footer

showEntriesBalances' :: (TableFormat a) => BalanceQuery -> Bool -> ChartOfAccounts -> a -> Amount -> [Ext (Balance Checked)] -> String
showEntriesBalances' bqry showCurrs coa fmt totals list =
  let l = map (showB' (maxFieldWidth fmt) showCurrs bqry coa (getCurrency totals)) list
      footer = showFooter fmt $ "    TOTALS: " ++ show totals
  in  unlines $
      tableGrid fmt [(ALeft,  ["DATE"]),
                     (ALeft,  ["DEBIT ACCOUNT"]),
                     (ARight, ["AMOUNT"]),
                     (ALeft,  ["CREDIT ACCOUNT"]),
                     (ARight, ["AMOUNT"]),
                     (ARight, ["BALANCE B/D"])] l ++ footer

causedByExt :: Ext (Balance Checked) -> Maybe (Ext (Entry Decimal Checked))
causedByExt (Ext date i pos attrs p) =
  Ext date i pos attrs <$> causedBy p

getEntries :: (Throws InternalError l,
               HasBalances a)
           => a
           -> Ledger l [Ext (Entry Decimal Checked)]
getEntries acc = do
  balances <- readIOListL (accountBalances acc)
  return $ mapMaybe causedByExt balances

