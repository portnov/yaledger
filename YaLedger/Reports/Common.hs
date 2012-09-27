{-# LANGUAGE GADTs, FlexibleContexts #-}
{- # OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.Common where

import Control.Applicative ((<$>))
import Control.Monad.Exception
import Control.Monad.Loc
import Data.Maybe
import Data.Decimal

import YaLedger.Types
import YaLedger.Strings
import YaLedger.Pretty
import YaLedger.Monad
import YaLedger.Exceptions

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

