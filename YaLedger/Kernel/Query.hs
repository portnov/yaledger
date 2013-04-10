{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}

module YaLedger.Kernel.Query
  (matchT, isOptional,
   match, matchAll,
   checkQuery, checkRecord,
   isAdmin
  ) where

import qualified Data.Map as M

import YaLedger.Types

-- | Match account type
matchT :: PostingType -> AccountGroupType -> Bool
matchT _       AGFree   = True
matchT ECredit AGCredit = True
matchT EDebit  AGDebit  = True
matchT _       _        = False

-- | Check if attribute value is optional
isOptional :: AttributeValue -> Bool
isOptional (Optional _) = True
isOptional _            = False

-- | Match attributes set
match :: Attributes  -- ^ Set of attributes (of account, for example)
      -> Attributes  -- ^ Attributes query (attributes of entry, for example)
      -> Bool
match attrs qry =
  let check (name, value) = case M.lookup name attrs of
                              Nothing -> isOptional value
                              Just av  -> matchAV value av
  in  all check $ M.assocs qry

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

