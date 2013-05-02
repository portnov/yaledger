{-# LANGUAGE EmptyDataDecls, GADTs, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeSynonymInstances, DeriveDataTypeable, RecordWildCards, ScopedTypeVariables, MultiParamTypeClasses, StandaloneDeriving #-}

module YaLedger.Kernel.Balances where

import Control.Applicative
import Data.Decimal

import YaLedger.Types.Common
import YaLedger.Types.Ledger

zeroBalance :: Balance Checked
zeroBalance = Balance Nothing 0 0 0

noChecks :: BalanceChecks
noChecks = BalanceChecks Nothing Nothing Nothing

getBalanceInfo :: BalanceQuery -> Balance Checked -> BalanceInfo Decimal
getBalanceInfo (Only AvailableBalance) b =
  BalanceInfo (Just $ balanceValue b + debitHolds b) Nothing
getBalanceInfo (Only LedgerBalance) b =
  BalanceInfo Nothing (Just $ balanceValue b + creditHolds b)
getBalanceInfo BothBalances b =
  BalanceInfo (Just $ balanceValue b + debitHolds b) (Just $ balanceValue b + creditHolds b)

-- | Empty BalanceInfo
noBalanceInfo :: BalanceInfo v
noBalanceInfo = BalanceInfo Nothing Nothing

-- | Check if balance info is not zero
isNotZeroBI :: BalanceInfo Amount -> Bool
isNotZeroBI (BalanceInfo (Just x) Nothing) = isNotZero x
isNotZeroBI (BalanceInfo Nothing (Just x)) = isNotZero x
isNotZeroBI (BalanceInfo (Just x) (Just y)) = isNotZero x || isNotZero y
isNotZeroBI bi = error $ "Impossible: isNotZeroBI: " ++ show bi

absBI :: BalanceInfo Amount -> BalanceInfo Amount
absBI bi = bi {
            biAvailable = absAmount <$> biAvailable bi,
            biLedger    = absAmount <$> biLedger bi }

-- | Set one of balances in BalanceInfo
setBalanceInfo :: BalanceType -> v -> BalanceInfo v -> BalanceInfo v
setBalanceInfo AvailableBalance x bi = bi {biAvailable = Just x}
setBalanceInfo LedgerBalance    x bi = bi {biLedger    = Just x}

-- | Set currency info for balances info
balanceInfoSetCurrency :: BalanceInfo Decimal -> Currency -> BalanceInfo Amount
balanceInfoSetCurrency bi c =
  bi {biAvailable = (:# c) <$> biAvailable bi,
      biLedger    = (:# c) <$> biLedger    bi }

sumBalanceInfo :: Currency -> [BalanceInfo Decimal] -> BalanceInfo Amount
sumBalanceInfo c bis = balanceInfoSetCurrency (foldr add zero bis) c
  where
    zero = BalanceInfo (Just 0) (Just 0)
    add bi1 bi2 = BalanceInfo {
                    biAvailable = liftA2 (+) (biAvailable bi1) (biAvailable bi2),
                    biLedger    = liftA2 (+) (biLedger    bi1) (biLedger    bi2) }

balanceGetter :: BalanceType -> (Balance Checked -> Decimal)
balanceGetter LedgerBalance    b = balanceValue b + creditHolds b
balanceGetter AvailableBalance b = balanceValue b + debitHolds  b

