{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
-- | Functions for searching corresponding accounts
module YaLedger.Kernel.Correspondence
  (matchT, match, matchAll,
   isOptional,
   filterCoA, runCQuery,
   buildGroupsMap,
   groupIDs, lookupAMap
  ) where

import Control.Monad
import Data.Maybe
import qualified Data.Map as M

import YaLedger.Types
import YaLedger.Kernel.Common
import YaLedger.Kernel.Query
import YaLedger.Logger

-- | Number of matching optional attributes from given set
optionalAttributes :: Attributes  -- ^ Attributes query
                   -> AnyAccount
                   -> Int
optionalAttributes as a = 
    go (filter (\(_,value) -> isOptional value) $ M.assocs as) a
  where
    go []     _   = 1
    go ((k,v):as) acc =
      case M.lookup k (accountAttributes acc) of
        Nothing -> go as acc
        Just av -> if matchAV v av
                     then 1 + go as acc
                     else     go as acc

-- | Filter list of accounts by optional attributes
filterByAddAttributes :: Attributes   -- ^ Attributes query
                      -> [AnyAccount]
                      -> [AnyAccount]
filterByAddAttributes as accs =
  let scores = [(acc, optionalAttributes as acc) | acc <- accs]
      m      = maximum (map snd scores)
  in  [acc | (acc, score) <- scores, score == m]

-- | Filter chart of accounts using 'CQuery'
filterCoA :: LedgerOptions -> CQuery -> ChartOfAccounts -> [AnyAccount]
filterCoA opts qry@(CQuery {..}) (Branch {..}) = concatMap (filterCoA opts qry) branchChildren

filterCoA opts (CQuery {..}) (Leaf {..}) =
    if (getID leafData `notElem` cqExcept) &&
       ((matchT opts cqType (accountType leafData) (accountAttributes leafData)) ||
        (accountType leafData == AGFree))
      then if (getCurrency leafData `elem` cqCurrencies) &&
              (accountAttributes leafData `match` cqAttributes)
             then [leafData]
             else []
      else []

-- | Search an account by 'CQuery'
runCQuery :: LedgerOptions -> CQuery -> ChartOfAccounts -> Maybe AnyAccount
runCQuery opts qry coa =
  case filterCoA opts qry coa of
    []  -> Nothing
    [x] -> Just x
    list -> $trace ("runCQuery: multiple choice: " ++ show list) $
            Just $ head $ filterByAddAttributes (cqAttributes qry) list

-- | List of groups IDs of all account's parent groups
groupIDs :: ChartOfAccounts
         -> AccountID         -- ^ Account ID
         -> Maybe [GroupID] -- ^ Groups IDs
groupIDs tree i = go [] i tree
  where
    go :: [GroupID] -> AccountID -> ChartOfAccounts -> Maybe [GroupID]
    go xs i (Branch _ ag children)
      | i `inRange` agRange ag = do
        let accs = [acc | Leaf _ acc <- children]
        case filter (\a -> getID a == i) accs of
          [_] -> return (agID ag: xs)
          _   -> let grps = [map (go (agID ag: agID ag': xs) i) grp | Branch _ ag' grp <- children]
                 in  msum $ concat grps
      | otherwise = Nothing
    go xs i (Leaf _ acc)
      | getID acc == i = Just xs
      | otherwise      = Nothing

buildGroupsMap :: ChartOfAccounts -> [AccountID] -> M.Map AccountID [GroupID]
buildGroupsMap coa aids = M.fromList [(i, grps) | (i, Just grps) <- zip aids (map (groupIDs coa) aids)]

-- | Lookup for corresponding account by account map
lookupAMap :: LedgerOptions
           -> M.Map AccountID [GroupID]
           -> ChartOfAccounts
           -> AccountMap
           -> CQuery
           -> [AccountID]       -- ^ Account IDs
           -> Maybe AnyAccount
lookupAMap opts groupsMap coa amap qry is = listToMaybe $ catMaybes $ concat [map (good i) amap | i <- is]
  where
    groupIDs' :: AccountID -> [GroupID]
    groupIDs' aid = fromMaybe [] $ M.lookup aid groupsMap
    
    good :: AccountID -> AMEntry -> Maybe AnyAccount
    good i (AMAccount j :=> ToCoA r)
      | i == j    = runCQuery opts qry r
      | otherwise = Nothing
    good i (AMAccount j :=> ToAttributes as)
      | i == j    = runCQuery opts (qry {cqAttributes = as `M.union` cqAttributes qry}) coa
      | otherwise = Nothing
    good i (AMGroup g :=> ToCoA r) =
      let gids = groupIDs' i
      in  if g `elem` gids
            then runCQuery opts qry r
            else Nothing
    good i (AMGroup g :=> ToAttributes as) =
      let gids = groupIDs' i
          qry' = qry {cqAttributes = as `M.union` cqAttributes qry}
      in  if g `elem` gids
            then runCQuery opts qry' coa
            else Nothing
    good _ (AMAttributes as :=> ToCoA r)
      | cqAttributes qry `matchAll` as = runCQuery opts qry r
      | otherwise = Nothing
    good _ (AMAttributes as :=> ToAttributes as')
      | cqAttributes qry `matchAll` as =
            let attrs = as' `M.union` cqAttributes qry
            in  runCQuery opts (qry {cqAttributes = attrs}) coa
      | otherwise = Nothing

