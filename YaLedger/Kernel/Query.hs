{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}

module YaLedger.Kernel.Query
  (matchT, isOptional,
   match, matchAll,
   checkQuery, checkRecord,
   isAdmin
  ) where

import qualified Data.Map as M

import YaLedger.Types
import YaLedger.Kernel.Classification

-- | Match account type
matchT :: LedgerOptions -> AccountAction -> AccountGroupType -> Attributes -> Bool
matchT opts ToIncrease k attrs = k `elem` [AGDebit,  AGFree]
matchT opts ToDecrease k attrs = k `elem` [AGCredit, AGFree]

-- | Match all attributes
matchAll :: Attributes -- ^ Set of attributes (of account, for example)
         -> Attributes -- ^ Attributes query (attributes of entry, for example)
         -> Bool
matchAll attrs qry =
  let check (name, value) = case M.lookup name qry of
                              Nothing  -> isOptional value
                              Just av  -> matchAV value av
  in  all check (M.assocs attrs) && all (`elem` M.keys attrs) (M.keys qry)

-- | Check if record \/ entry \/ whatever matches to query
checkQuery :: Query -> Ext a -> Bool
checkQuery (Query {..}) (Ext {..}) =
  let p = case qStart of
            Just s  -> getDate > s
            Nothing -> True

      q = case qEnd of
            Just e -> getDate <= e
            Nothing -> True

      r = all matches $ M.assocs qAttributes

      matches (name, avalue) =
        case M.lookup name getAttributes of
          Nothing  -> False
          Just val -> matchAV val avalue

  in  p && q && r

-- | Similar to 'checkQuery', but this is True for all
-- admin records
checkRecord :: Query -> Ext Record -> Bool
checkRecord qry rec =
    isAdmin (getContent rec) || checkQuery qry rec

-- | Check if record is administrative
-- (non-financial)
isAdmin :: Record -> Bool
isAdmin (Transaction _) = False
isAdmin _               = True

