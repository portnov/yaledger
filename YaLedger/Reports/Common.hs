{-# LANGUAGE GADTs #-}

module YaLedger.Reports.Common where

import Data.Decimal

import YaLedger.Types
import YaLedger.Strings
import YaLedger.Pretty

showE :: Ext (Entry Decimal Checked) -> [[String]]
showE (Ext {getDate = date, getContent = (CEntry dt cr rd)}) =
    [prettyPrint date: replicate (m-1) "",
     map posting cr, map posting dt, rdS]
  where
    m = max (length cr) (length dt)
    rdS
      | rd == OneCurrency = []
      | otherwise = [show rd]

    posting :: Posting Decimal t -> String
    posting (DPosting acc x) = getName acc ++ ": " ++ show (roundTo 4 x) ++ getCurrency acc
    posting (CPosting acc x) = getName acc ++ ": " ++ show (roundTo 4 x) ++ getCurrency acc

showEntries :: Amount -> [Ext (Entry Decimal Checked)] -> String
showEntries totals list =
  let l = map showE list
      footer = ["    TOTALS: " ++ show totals]
  in  unlines $
          grid ["DATE", "CREDIT", "DEBIT", "RATES DIFF."] l ++ footer

