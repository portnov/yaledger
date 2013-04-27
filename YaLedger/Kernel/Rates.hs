{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}

module YaLedger.Kernel.Rates where

import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Monad.Exception
import qualified Data.Map as M
import Data.Maybe
import Data.Decimal
import Data.Dates

import YaLedger.Types
import YaLedger.Exceptions

-- | Lookup for active exchange rate
lookupRate :: (Monad m,
               Throws NoSuchRate l)
           => Maybe DateTime    -- ^ Date to search exchange rate for. Nothing for current date.
           -> Currency          -- ^ Source currency
           -> Currency          -- ^ Target currency
           -> LedgerT l m Double
lookupRate mbDate from to = do
    now <- gets lsStartDate
    let date = fromMaybe now mbDate
    rates <- gets lsRates
    let goodRates = [getContent rate | rate <- rates, getDate rate <= date]
    case go goodRates from to goodRates of
      Nothing -> throwP (NoSuchRate from to)
      Just rate -> return rate
  where
    go _ _ _ [] = Nothing
    go ar f t (Explicit cFrom aFrom cTo aTo rev: rs)
      | (cFrom == f) && (cTo == t) = Just (aTo / aFrom)
      | rev && (cTo == f) && (cFrom == t) = Just (aFrom / aTo)
      | otherwise = go ar f t rs
    go ar f t (Implicit cFrom cTo cBase rev: rs)
      | (cFrom == f) && (cTo == t) = do
            x <- go ar f cBase ar
            y <- go ar t cBase ar
            return (x / y)
      | rev && (cFrom == t) && (cTo == f) = do
            x <- go ar f cBase ar
            y <- go ar t cBase ar
            return (x / y)
      | otherwise = go ar f t rs

-- | Convert 'Amount' to another currency
convert :: (Monad m,
            Throws NoSuchRate l)
        => Maybe DateTime    -- ^ Date of which exchange rate should be used
        -> Currency          -- ^ Target currency
        -> Amount
        -> LedgerT l m Amount
convert mbDate c' (x :# c)
  | c == c' = return (x :# c)
  | otherwise = do
    rate <- lookupRate mbDate c c'
    -- Round amount to precision of target currency
    let qty = roundTo (fromIntegral $ cPrecision c') (x *. rate)
    return $ qty :# c'

convertDelta ::  (Monad m,
            Throws NoSuchRate l)
        => Maybe DateTime    -- ^ Date of which exchange rate should be used
        -> Currency          -- ^ Target currency
        -> Delta Amount
        -> LedgerT l m (Delta Amount)
convertDelta mbDate c (Increase x) = Increase `liftM` convert mbDate c x
convertDelta mbDate c (Decrease x) = Decrease `liftM` convert mbDate c x

convertBalanceInfo :: (Monad m,
                       Throws NoSuchRate l)
                   => Maybe DateTime
                   -> Currency
                   -> BalanceInfo Amount
                   -> LedgerT l m (BalanceInfo Decimal)
convertBalanceInfo mbDate c bi = do
  available <- case biAvailable bi of
                 Nothing -> return Nothing
                 Just x  -> (Just . (\(a :# _) -> a)) <$> convert mbDate c x
  ledger    <- case biLedger bi of
                 Nothing -> return Nothing
                 Just x  -> (Just . (\(a :# _) -> a)) <$> convert mbDate c x
  return $ BalanceInfo available ledger

-- | Convert a posting to another currency.
-- In returned posting, amount is in target currency.
convertPosting :: Throws NoSuchRate l
               => Maybe DateTime      -- ^ Date of exchange rates
               -> Currency            -- ^ Target currency
               -> Posting Amount t
               -> Ledger l (Posting Decimal t)
convertPosting mbDate to (DPosting acc a b) = do
  x :# _ <- convert mbDate to a
  return $ DPosting acc x b
convertPosting mbDate to (CPosting acc a b) = do
  x :# _ <- convert mbDate to a
  return $ CPosting acc x b

-- | Convert a posting to currency of it's account.
convertPosting' :: Throws NoSuchRate l
               => Maybe DateTime        -- ^ Date of exchange rates
               -> Posting Amount t
               -> Ledger l (Posting Decimal t)
convertPosting' mbDate (DPosting acc a b) = do
  x :# _ <- convert mbDate (getCurrency acc) a
  return $ DPosting acc x b
convertPosting' mbDate (CPosting acc a b) = do
  x :# _ <- convert mbDate (getCurrency acc) a
  return $ CPosting acc x b

convertAnyPosting :: Throws NoSuchRate l
               => Maybe DateTime        -- ^ Date of exchange rates
               -> AnyPosting Amount
               -> Ledger l (AnyPosting Decimal)
convertAnyPosting mbDate (DP (DPosting acc a b)) = do
  x :# _ <- convert mbDate (getCurrency acc) a
  return $ DP $ DPosting acc x b
convertAnyPosting mbDate (CP (CPosting acc a b)) = do
  x :# _ <- convert mbDate (getCurrency acc) a
  return $ CP $ CPosting acc x b

-- | Convert Posting Decimal. Returns only an amount in target currency.
convertDecimal :: Throws NoSuchRate l
               => Maybe DateTime
               -> Currency
               -> Posting Decimal t
               -> Ledger l Decimal
convertDecimal mbDate to (DPosting acc a _) = do
  x :# _ <- convert mbDate to (a :# getCurrency acc)
  return x
convertDecimal mbDate to (CPosting acc a _) = do
  x :# _ <- convert mbDate to (a :# getCurrency acc)
  return x

