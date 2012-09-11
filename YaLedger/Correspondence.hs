{-# LANGUAGE RecordWildCards #-}

module YaLedger.Correspondence where

import Control.Monad
import Data.Maybe

import YaLedger.Types
import YaLedger.Tree

nonsignificantAttributes :: [String]
nonsignificantAttributes =
  ["description"]

data CQuery = CQuery {
  cqType :: PostingType,
  cqCurrency :: [Currency],
  cqAttributes :: Attributes }
  deriving (Eq, Show)

matchT :: PostingType -> AccountGroupType -> Bool
matchT _       AGFree   = True
matchT ECredit AGCredit = True
matchT EDebit  AGDebit  = True
matchT _       _        = False

matchA :: Attributes -> Attributes -> Bool
matchA attrs qry =
  let qry' = filter (\(name,_) -> name `notElem` nonsignificantAttributes) qry
  in  all (`elem` attrs) qry'

first :: (a -> Maybe b) -> [a] -> Maybe b
first _ [] = Nothing
first fn (x:xs) =
    case fn x of
      Just y  -> Just y
      Nothing -> first fn xs

runCQuery :: CQuery -> AccountPlan -> Maybe AnyAccount
runCQuery qry@(CQuery {..}) (Branch {..}) =
  if (cqType `matchT` agType branchData) || (agType branchData == AGFree)
    then first (runCQuery qry) branchChildren
    else Nothing

runCQuery qry@(CQuery {..}) (Leaf {..}) =
    if (cqType `matchT` accountType leafData) || (accountType leafData == AGFree)
      then if (getCurrency leafData `elem` cqCurrency) &&
              accountAttributes leafData `matchA` cqAttributes
             then Just leafData
             else Nothing
      else Nothing

inRange :: Integer -> (Integer, Integer) -> Bool
inRange i (m, n) = (m < i) && (i <= n)

accountByID :: Integer -> AccountPlan -> Maybe AnyAccount
accountByID i (Branch _ _ ag children)
  | i `inRange` agRange ag = do
      let accs = [acc | Leaf _ _ acc <- children]
      case filter (\a -> getID a == i) accs of
        [x] -> return x
        _   -> do
               let grps = [grp | Branch _ _ _ grp <- children]
               first (accountByID i) (concat grps)
  | otherwise = Nothing

accountByID i (Leaf _ _ acc)
  | getID acc == i = Just acc
  | otherwise      = Nothing

-- | List of groups IDs of all account's parent groups
groupIDs :: Integer         -- ^ Account ID
         -> AccountPlan
         -> Maybe [Integer] -- ^ Groups IDs
groupIDs i tree = go [] i tree
  where
    go :: [Integer] -> Integer -> AccountPlan -> Maybe [Integer]
    go xs i (Branch _ _ ag children)
      | i `inRange` agRange ag = do
        let accs = [acc | Leaf _ _ acc <- children]
        case filter (\a -> getID a == i) accs of
          [x] -> return (agID ag: xs)
          _   -> let grps = [map (go (agID ag: agID ag': xs) i) grp | Branch _ _ ag' grp <- children]
                 in  msum $ concat grps
      | otherwise = Nothing
    go xs i (Leaf _ _ acc)
      | getID acc == i = Just xs
      | otherwise      = Nothing

-- | Lookup for corresponding account by account map
lookupAMap :: AccountPlan
           -> AccountMap
           -> CQuery
           -> [Integer]       -- ^ Account IDs
           -> Maybe AnyAccount
lookupAMap plan amap qry is = msum [first (good i) amap | i <- is]
  where
    good i (AMAccount j :=> r)
      | i == j    = runCQuery qry r
      | otherwise = Nothing
    good i (AMGroup g :=> r) =
      let gids = fromMaybe [] $ groupIDs i plan
      in  if g `elem` gids
            then runCQuery qry r
            else Nothing
