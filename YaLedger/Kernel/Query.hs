{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}

module YaLedger.Kernel.Query where

import qualified Data.Map as M

import YaLedger.Types

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

