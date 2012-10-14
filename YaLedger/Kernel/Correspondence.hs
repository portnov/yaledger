{-# LANGUAGE RecordWildCards #-}
-- | Functions for searching corresponding accounts
module YaLedger.Kernel.Correspondence
  (matchT, match, matchAll,
   isOptional,
   filterCoA, runCQuery,
   groupIDs, lookupAMap,
   inRange
  ) where

import Control.Monad
import Data.Maybe
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
filterCoA :: CQuery -> ChartOfAccounts -> [AnyAccount]
filterCoA qry@(CQuery {..}) (Branch {..}) =
    if (cqType `matchT` agType branchData) || (agType branchData == AGFree)
      then concatMap (filterCoA qry) branchChildren
      else []

filterCoA (CQuery {..}) (Leaf {..}) =
    if (getID leafData `notElem` cqExcept) &&
       ((cqType `matchT` accountType leafData) ||
        (accountType leafData == AGFree))
      then if (getCurrency leafData `elem` cqCurrencies) &&
              (accountAttributes leafData `match` cqAttributes)
             then [leafData]
             else []
      else []

-- | Search an account by 'CQuery'
runCQuery :: CQuery -> ChartOfAccounts -> Maybe AnyAccount
runCQuery qry coa =
  case filterCoA qry coa of
    []  -> Nothing
    [x] -> Just x
    list -> Just $ head $ filterByAddAttributes (cqAttributes qry) list

inRange :: Integer -> (Integer, Integer) -> Bool
inRange i (m, n) = (m < i) && (i <= n)

-- | List of groups IDs of all account's parent groups
groupIDs :: AccountID         -- ^ Account ID
         -> ChartOfAccounts
         -> Maybe [GroupID] -- ^ Groups IDs
groupIDs i tree = go [] i tree
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

-- | Lookup for corresponding account by account map
lookupAMap :: ChartOfAccounts
           -> AccountMap
           -> CQuery
           -> [AccountID]       -- ^ Account IDs
           -> Maybe AnyAccount
lookupAMap coa amap qry is = listToMaybe $ catMaybes $ concat [map (good i) amap | i <- is]
  where
    good :: AccountID -> AMEntry -> Maybe AnyAccount
    good i (AMAccount j :=> ToCoA r)
      | i == j    = runCQuery qry r
      | otherwise = Nothing
    good i (AMAccount j :=> ToAttributes as)
      | i == j    = runCQuery (qry {cqAttributes = as `M.union` cqAttributes qry}) coa
      | otherwise = Nothing
    good i (AMGroup g :=> ToCoA r) =
      let gids = fromMaybe [] $ groupIDs i coa
      in  if g `elem` gids
            then runCQuery qry r
            else Nothing
    good i (AMGroup g :=> ToAttributes as) =
      let gids = fromMaybe [] $ groupIDs i coa
          qry' = qry {cqAttributes = as `M.union` cqAttributes qry}
      in  if g `elem` gids
            then runCQuery qry' coa
            else Nothing
    good _ (AMAttributes as :=> ToCoA r)
      | cqAttributes qry `matchAll` as = runCQuery qry r
      | otherwise = Nothing
    good _ (AMAttributes as :=> ToAttributes as')
      | cqAttributes qry `matchAll` as =
            let attrs = as' `M.union` cqAttributes qry
            in  runCQuery (qry {cqAttributes = attrs}) coa
      | otherwise = Nothing

