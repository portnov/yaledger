{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
-- | Accounts classification
module YaLedger.Kernel.Classification where

import Control.Applicative ((<$>))
import Control.Monad.State
import qualified Data.Map as M

import YaLedger.Types

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

-- | Match attributes set
match :: Attributes  -- ^ Set of attributes (of account, for example)
      -> Attributes  -- ^ Attributes query (attributes of entry, for example)
      -> Bool
match attrs qry =
  let check (name, value) = case M.lookup name attrs of
                              Nothing -> isOptional value
                              Just av  -> matchAV value av
  in  all check $ M.assocs qry

-- | Check if attribute value is optional
isOptional :: AttributeValue -> Bool
isOptional (Optional _) = True
isOptional _            = False

