{-# LANGUAGE GADTs #-}

module YaLedger.Reports.Common where

import Data.Decimal

import YaLedger.Types
import YaLedger.Strings

showE :: Ext (Entry Decimal Checked) -> ([String], [String], [String])
showE (Ext {getContent = (CEntry dt cr rd)}) =
    (map posting dt, map posting cr, rdS)
  where
    rdS
      | rd == OneCurrency = []
      | otherwise = [show rd]
    posting (DPosting acc x) = getName acc ++ ": " ++ show x
    posting (CPosting acc x) = getName acc ++ ": " ++ show x

showEntries :: Amount -> [Ext (Entry Decimal Checked)] -> String
showEntries totals list =
  let l = map showE list
      cr = [x | (x,_,_) <- l]
      dt = [x | (_,x,_) <- l]
      rd = [x | (_,_,x) <- l]
      ms = [maximum (map length [l1,l2,l3]) | (l1,l2,l3) <- l]
      padE n x = x ++ replicate (n - length x) ""
      cr' = concat $ zipWith padE ms cr
      dt' = concat $ zipWith padE ms dt
      rd' = concat $ zipWith padE ms rd
      footer = ["    TOTALS: " ++ show totals]
      h1 = "CREDIT"
      h2 = "DEBIT"
      h3 = "RATES DIFF."
      m1 = maximum (map length (h1:cr'))
      m2 = maximum (map length (h2:dt'))
      m3 = maximum (map length (h3:rd'))
      s1 = replicate (m1+2) '='
      s2 = replicate (m2+2) '='
      s3 = replicate (m3+2) '='
      (h1':cr'') = alignMax ACenter (h1:cr')
      (h2':dt'') = alignMax ACenter (h2:dt')
      (h3':rd'') = alignMax ACenter (h3:rd')
      go l1 l2 l3 = columns' [h1':s1:l1, h2':s2:l2, h3':s3:l3]
  in unlines (go cr'' dt'' rd'' ++ footer)

