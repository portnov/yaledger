{-# LANGUAGE RecordWildCards #-}

module Correspondence where

import Types
import Tree

data CQuery = CQuery {
  cqType :: EntryType,
  cqCurrency :: [Currency],
  cqAttributes :: Attributes }
  deriving (Eq, Show)

matchT :: EntryType -> AccountGroupType -> Bool
matchT _       AGFree   = True
matchT ECredit AGCredit = True
matchT EDebit  AGDebit  = True

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
              all (`elem` accountAttributes leafData) cqAttributes
             then Just leafData
             else Nothing
      else Nothing

accountByID :: Integer -> AccountPlan -> Maybe AnyAccount
accountByID i (Branch _ _ ag children) = do
  let (m,n) = agRange ag
  if (m < i) && (i <= n)
    then do
      let accs = [acc | Leaf _ _ acc <- children]
      case filter (\a -> getID a == i) accs of
        [x] -> return x
        _   -> do
               let grps = [grp | Branch _ _ _ grp <- children]
               first (accountByID i) (concat grps)
    else Nothing
accountByID i (Leaf _ _ acc) =
  if getID acc == i
    then Just acc
    else Nothing

