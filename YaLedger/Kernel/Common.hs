{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
-- | This module contains kernel functions, which do not
-- require Ledger monad.
module YaLedger.Kernel.Common
  (inRange, first, whenJust,
   filterByAccountType,
   isInCoA,
   getCoAItem, getCoAItemT,
   getAccount, getAccountT,
   accountFullPath,
   accountFullPath',
   autoPosting
  ) where

import Control.Monad
import Control.Failure
import Data.List (intercalate)
import Text.Printf

import YaLedger.Types
import YaLedger.Exceptions
import YaLedger.Kernel.Classification

inRange :: Integer -> (Integer, Integer) -> Bool
inRange i (m, n) = (m < i) && (i <= n)

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing  _  = return ()
whenJust (Just a) fn = fn a

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
    check AGCredit (Leaf {leafData = WCredit _}) = True
    check AGCredit (Leaf {leafData = WFree   _}) = True
    check AGDebit  (Leaf {leafData = WDebit  _}) = True
    check AGDebit  (Leaf {leafData = WFree   _}) = True
    check AGFree   (Leaf {leafData = WFree   _}) = True
    check t b@(Branch {}) = agType (branchData b) == t
    check _ _ = False

-- | Check if account belongs to chart of accounts
isInCoA :: AnyAccount -> ChartOfAccounts -> Bool
isInCoA acc l@(Leaf {}) = getID acc == getID (leafData l)
isInCoA acc b@(Branch {}) = getID acc `inRange` agRange (branchData b)

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

autoPosting :: forall v m. (Show v, Monad m)
            => LedgerOptions
            -> Delta v
            -> Path        -- ^ Path to account
            -> AnyAccount
            -> HoldUsage
            -> m (AnyPosting v)
autoPosting opts amt accPath acc use = do
    let attrs = accountAttributes acc
    if isAssets opts attrs
      then case (amt, acc) of
             (Increase am, WFree a)   -> return $ DP $ DPosting (Left a)  am use
             (Decrease am, WFree a)   -> return $ CP $ CPosting (Left a)  am use
             (Increase am, WDebit a)  -> return $ DP $ DPosting (Right a) am use
             (Decrease am, WCredit a) -> return $ CP $ CPosting (Right a) am use
             (_,_) -> fail $ printfErr accPath amt acc
      else case (amt, acc) of
             (Increase am, WFree a)   -> return $ CP $ CPosting (Left a)  am use
             (Decrease am, WFree a)   -> return $ DP $ DPosting (Left a)  am use
             (Increase am, WCredit a) -> return $ CP $ CPosting (Right a) am use
             (Decrease am, WDebit a)  -> return $ DP $ DPosting (Right a) am use
             (_,_) -> fail $ printfErr accPath amt acc
  where
    printfErr :: Path -> Delta v -> AnyAccount -> String
    printfErr path amt acc = printf "Invalid account type %s: %s instead of %s (amount: %s)"
                                    (intercalate "/" path) signAmt signAcc (show amt)
      where

        signAmt :: String
        signAmt = case amt of
                    Decrease _  -> "decrease"
                    Increase _  -> "increase"

        signAcc :: String
        signAcc = case acc of
                    WFree _   -> "free"
                    WCredit _ -> "credit"
                    WDebit _  -> "debit"

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

accountFullPath' :: (Monad m, HasID a) => String -> a -> ChartOfAccounts -> m Path
accountFullPath' msg acc coa =
  case accountFullPath (getID acc) coa of
    Nothing -> fail $ "Impossible: " ++ msg ++ ": No such account: " ++ show (getID acc)
    Just p  -> return p

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

