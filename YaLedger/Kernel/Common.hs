{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
-- | This module contains kernel functions, which do not
-- require Ledger monad.
module YaLedger.Kernel.Common
  (inRange, first,
   filterByAccountType,
   getCoAItem, getCoAItemT,
   getAccount, getAccountT,
   accountFullPath
  ) where

import Control.Monad
import Control.Failure

import YaLedger.Types
import YaLedger.Exceptions

inRange :: Integer -> (Integer, Integer) -> Bool
inRange i (m, n) = (m < i) && (i <= n)

first :: (a -> Maybe b) -> [a] -> Maybe b
first _ [] = Nothing
first fn (x:xs) =
    case fn x of
      Just y  -> Just y
      Nothing -> first fn xs

-- | Filter list of CoA items by account type
filterByAccountType :: AccountGroupType -> [ChartOfAccounts] -> [ChartOfAccounts]
filterByAccountType t coas = filter (check t) coas
  where
    check AGCredit (Leaf {leafData = WCredit _ _}) = True
    check AGCredit (Leaf {leafData = WFree   _ _}) = True
    check AGDebit  (Leaf {leafData = WDebit  _ _}) = True
    check AGDebit  (Leaf {leafData = WFree   _ _}) = True
    check AGFree   (Leaf {leafData = WFree   _ _}) = True
    check t b@(Branch {}) = agType (branchData b) == t
    check _ _ = False

getCoAItem :: (Monad m,
               Failure InvalidPath m)
           => (m SourcePos)
           -> (m ChartOfAccounts)
           -> Path
           -> m ChartOfAccounts
getCoAItem getPos fn path = do
  coa <- fn
  pos <- getPos
  case search' coa path of
    [] -> failure (InvalidPath path [] pos)
    [a] -> return a
    as -> failure (InvalidPath path as pos)

getCoAItemT :: (Monad m,
                Failure InvalidPath m)
            => AccountGroupType
            -> (m SourcePos)
            -> (m ChartOfAccounts)
            -> Path
            -> m ChartOfAccounts
getCoAItemT t getPos fn path = do
  coa <- fn
  pos <- getPos
  case search' coa path of
    [] -> failure (InvalidPath path [] pos)
    [a] -> return a
    as -> case filterByAccountType t as of
            []  -> failure (InvalidPath path [] pos)
            [a] -> return a
            as  -> failure (InvalidPath path as pos)

getAccount :: (Monad m,
               Failure InvalidPath m,
               Failure NotAnAccount m)
           => (m SourcePos)
           -> (m ChartOfAccounts)
           -> Path
           -> m AnyAccount
getAccount getPos fn path = do
  x <- getCoAItem getPos fn path
  pos <- getPos
  case x of
    Leaf {} -> return (leafData x)
    _ -> failure (NotAnAccount path pos)

getAccountT :: (Monad m,
               Failure InvalidPath m,
               Failure NotAnAccount m)
           => AccountGroupType
           -> (m SourcePos)
           -> (m ChartOfAccounts)
           -> Path
           -> m AnyAccount
getAccountT t getPos fn path = do
  x <- getCoAItemT t getPos fn path
  pos <- getPos
  case x of
    Leaf {} -> return (leafData x)
    _ -> failure (NotAnAccount path pos)

-- | Get fully qualfiied path of account
accountFullPath :: AccountID -> ChartOfAccounts -> Maybe Path
accountFullPath i tree = go [] i tree
  where
    go p i (Branch groupName ag children)
      | i `inRange` agRange ag = do
          let accs = [(getID acc, n) | Leaf n acc <- children]
          case filter ((== i) . fst) accs of
            [(_,accName)] -> return (p ++ [groupName, accName])
            _   -> do
                   let grps = [(n,grp) | Branch n _ grp <- children]
                   msum $ map (\(n,g) -> first (go (p ++ [groupName, n]) i) g) grps
      | otherwise = Nothing

    go p i (Leaf name acc)
      | getID acc == i = Just (p ++ [name])
      | otherwise      = Nothing

-- | Get fully qualfiied path of accounts group
groupFullPath :: GroupID -> ChartOfAccounts -> Maybe Path
groupFullPath i tree = go [] i tree
  where
    go p i (Branch groupName ag children)
      | i `inRange` agRange ag = do
          let grps = [(getID ag, n) | Branch n ag _ <- children]
          case filter ((== i) . fst) grps of
            [(_, name)] -> return (p ++ [groupName, name])
            _   -> do
                   let grps = [(n,grp) | Branch n _ grp <- children]
                   msum $ map (\(n,g) -> first (go (p ++ [groupName, n]) i) g) grps
      | otherwise = Nothing

    go p i (Leaf name acc) = Nothing

