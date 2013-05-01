{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
-- | This module contains kernel functions, which do not
-- require Ledger monad.
module YaLedger.Kernel.Common
  (CMonad (..),
   inRange, first, whenJust,
   filterByAccountType,
   isInCoA,
   getCoAItem, getCoAItemT,
   getAccount, getAccountT,
   getAccountS,
   accountFullPath,
   accountFullPath',
   autoPosting
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Failure
import Data.List (intercalate)
import Text.Printf

import YaLedger.Types
import YaLedger.Exceptions
import YaLedger.Kernel.Classification

class Monad m => CMonad m where
  cGetPosition :: m SourcePos
  cGetCoA :: m ChartOfAccounts

instance Monad m => CMonad (EMT l (LedgerStateT m)) where
  cGetPosition = gets lsPosition
  cGetCoA = gets lsCoA

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

getCoAItems :: (CMonad m,
               Failure InvalidPath m)
           => Path
           -> m [ChartOfAccounts]
getCoAItems path = do
  coa <- cGetCoA
  return $ search' coa path

getCoAItem :: (CMonad m,
               Failure InvalidPath m)
           => Path
           -> m ChartOfAccounts
getCoAItem path = do
  coa <- cGetCoA
  pos <- cGetPosition
  case search' coa path of
    [] -> failure (InvalidPath path [] pos)
    [a] -> return a
    as -> failure (InvalidPath path as pos)

getCoAItemT :: (CMonad m,
                Failure InvalidPath m)
            => AccountGroupType
            -> Path
            -> m ChartOfAccounts
getCoAItemT t path = do
  coa <- cGetCoA
  pos <- cGetPosition
  case search' coa path of
    [] -> failure (InvalidPath path [] pos)
    [a] -> return a
    as -> case filterByAccountType t as of
            []  -> failure (InvalidPath path [] pos)
            [a] -> return a
            as  -> failure (InvalidPath path as pos)

getAccount :: (CMonad m,
               Failure InvalidPath m,
               Failure NotAnAccount m)
           => Path
           -> m AnyAccount
getAccount path = do
  x <- getCoAItem path
  pos <- cGetPosition
  case x of
    Leaf {} -> return (leafData x)
    _ -> failure (NotAnAccount path pos)

getAccountT :: (CMonad m,
               Failure InvalidPath m,
               Failure NotAnAccount m)
           => AccountGroupType
           -> Path
           -> m AnyAccount
getAccountT t path = do
  x <- getCoAItemT t path
  pos <- cGetPosition
  case x of
    Leaf {} -> return (leafData x)
    _ -> failure (NotAnAccount path pos)

getAccountS :: (CMonad m,
               Failure InvalidPath m,
               Failure NotAnAccount m)
            => LedgerOptions
            -> Delta v
            -> Path
            -> m AnyAccount
getAccountS opts delta path = do
    items <- getCoAItems path
    pos <- cGetPosition
    case items of
      [] -> failure (InvalidPath path [] pos)
      [Leaf {leafData=a}] -> return a
      as -> let accs = [a | Leaf {leafData=a} <- as]
            in case filter checkDelta accs of
                [] -> failure (InvalidPath path [] pos)
                [a] -> return a
                as -> failure (InvalidPath path (map mkcoa as) pos)
  where
    checkDelta acc
      | isAssets opts (accountAttributes acc) =
          case (delta, acc) of
             (Increase am, WFree a)   -> True
             (Decrease am, WFree a)   -> True
             (Increase am, WDebit a)  -> True
             (Decrease am, WCredit a) -> True
             (_,_) -> False
      | otherwise =
          case (delta, acc) of
             (Increase am, WFree a)   -> True
             (Decrease am, WFree a)   -> True
             (Increase am, WCredit a) -> True
             (Decrease am, WDebit a)  -> True
             (_,_) -> False

    mkcoa a = Leaf (getName a) a


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

