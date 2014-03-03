{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, TemplateHaskell #-}

module YaLedger.Kernel.Rates where

import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Monad.Exception
import Data.Maybe
import Data.Decimal
import Data.Dates

import YaLedger.Types
import YaLedger.Exceptions

import Debug.Trace

-- | Lookup for active exchange rate
lookupRate :: (Monad m,
               Throws NoSuchRate l)
           => Maybe DateTime    -- ^ Date to search exchange rate for. Nothing for current date.
           -> RateGroupName            -- ^ Name of rates group
           -> Currency          -- ^ Source currency
           -> Currency          -- ^ Target currency
           -> LedgerT l m Double
lookupRate mbDate rgroup from to = do
    now <- gets lsStartDate
    let date = fromMaybe now mbDate
    rates <- gets (reverse . getRates rgroup . lsRates)
    let goodRatesD = [rate | rate <- rates, getDate rate <= date]
        goodRates = map getContent goodRatesD
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
        -> RateGroupName     -- ^ Name of rates group
        -> Currency          -- ^ Target currency
        -> Amount
        -> LedgerT l m Amount
convert mbDate rgroup c' (x :# c)
  | c == c' = return (x :# c)
  | otherwise = do
    rate <- lookupRate mbDate rgroup c c'
    -- Round amount to precision of target currency
    let qty = roundTo (fromIntegral $ cPrecision c') (x *. rate)
    return $ qty :# c'

convertDelta ::  (Monad m,
            Throws NoSuchRate l)
        => Maybe DateTime    -- ^ Date of which exchange rate should be used
        -> RateGroupName     -- ^ Name of rates group
        -> Currency          -- ^ Target currency
        -> Delta Amount
        -> LedgerT l m (Delta Amount)
convertDelta mbDate rgroup c (Increase x) = Increase `liftM` convert mbDate rgroup c x
convertDelta mbDate rgroup c (Decrease x) = Decrease `liftM` convert mbDate rgroup c x

convertBalanceInfo :: (Monad m,
                       Throws NoSuchRate l)
                   => Maybe DateTime
                   -> RateGroupName
                   -> Currency
                   -> BalanceInfo Amount
                   -> LedgerT l m (BalanceInfo Decimal)
convertBalanceInfo mbDate rgroup c bi = do
  available <- case biAvailable bi of
                 Nothing -> return Nothing
                 Just x  -> (Just . (\(a :# _) -> a)) <$> convert mbDate rgroup c x
  ledger    <- case biLedger bi of
                 Nothing -> return Nothing
                 Just x  -> (Just . (\(a :# _) -> a)) <$> convert mbDate rgroup c x
  return $ BalanceInfo available ledger

-- | Convert a posting to another currency.
-- In returned posting, amount is in target currency.
convertPosting :: Throws NoSuchRate l
               => Maybe DateTime      -- ^ Date of exchange rates
               -> RateGroupName
               -> Currency            -- ^ Target currency
               -> Posting Amount t
               -> Ledger l (Posting Decimal t)
convertPosting mbDate rgroup to (DPosting acc a b) = do
  x :# _ <- convert mbDate rgroup to a
  return $ DPosting acc x b
convertPosting mbDate rgroup to (CPosting acc a b) = do
  x :# _ <- convert mbDate rgroup to a
  return $ CPosting acc x b

-- | Convert a posting to currency of it's account.
convertPosting' :: Throws NoSuchRate l
               => Maybe DateTime        -- ^ Date of exchange rates
               -> RateGroupName
               -> Posting Amount t
               -> Ledger l (Posting Decimal t)
convertPosting' mbDate rgroup (DPosting acc a b) = do
  x :# _ <- convert mbDate rgroup (getCurrency acc) a
  return $ DPosting acc x b
convertPosting' mbDate rgroup (CPosting acc a b) = do
  x :# _ <- convert mbDate rgroup (getCurrency acc) a
  return $ CPosting acc x b

convertAnyPosting :: Throws NoSuchRate l
               => Maybe DateTime        -- ^ Date of exchange rates
               -> RateGroupName
               -> AnyPosting Amount
               -> Ledger l (AnyPosting Decimal)
convertAnyPosting mbDate rgroup (DP (DPosting acc a b)) = do
  x :# _ <- convert mbDate rgroup (getCurrency acc) a
  return $ DP $ DPosting acc x b
convertAnyPosting mbDate rgroup (CP (CPosting acc a b)) = do
  x :# _ <- convert mbDate rgroup (getCurrency acc) a
  return $ CP $ CPosting acc x b

-- | Convert Posting Decimal. Returns only an amount in target currency.
convertDecimal :: Throws NoSuchRate l
               => Maybe DateTime
               -> RateGroupName
               -> Currency
               -> Posting Decimal t
               -> Ledger l Decimal
convertDecimal mbDate rgroup to (DPosting acc a _) = do
  x :# _ <- convert mbDate rgroup to (a :# getCurrency acc)
  return x
convertDecimal mbDate rgroup to (CPosting acc a _) = do
  x :# _ <- convert mbDate rgroup to (a :# getCurrency acc)
  return x

