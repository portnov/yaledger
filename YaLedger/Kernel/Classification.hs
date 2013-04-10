{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
-- | Accounts classification
module YaLedger.Kernel.Classification where

import Control.Applicative ((<$>))
import Control.Monad.State
import qualified Data.Map as M

import YaLedger.Types
import YaLedger.Kernel.Query (match)

checkConfiguredQuery :: LedgerOptions
                     -> (LedgerOptions -> Attributes)
                     -> Attributes -> Bool
checkConfiguredQuery opts fn =
    let qry = fn opts
    in  (`match` qry)

-- | Check if `a' belongs to assets
isAssets :: LedgerOptions -> Attributes -> Bool
isAssets opts = checkConfiguredQuery opts assetAccounts

isLiabilities :: LedgerOptions -> Attributes -> Bool
isLiabilities opts attrs =
  let qry = liabilityAccounts opts
  in if M.null qry
      then not $ isAssets opts attrs
      else attrs `match` qry

isIncomes :: LedgerOptions -> Attributes -> Bool
isIncomes opts = checkConfiguredQuery opts incomeAccounts

isExpences :: LedgerOptions -> Attributes -> Bool
isExpences opts = checkConfiguredQuery opts expenceAccounts

