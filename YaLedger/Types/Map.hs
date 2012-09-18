{-# LANGUAGE EmptyDataDecls, GADTs, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeSynonymInstances, DeriveDataTypeable, RecordWildCards, ScopedTypeVariables, MultiParamTypeClasses #-}

module YaLedger.Types.Map where

import YaLedger.Attributes
import YaLedger.Types.Common
import YaLedger.Types.Ledger

type AccountMap = [AMEntry]

data AMEntry = AMFrom :=> AMTo
  deriving (Eq)

instance Show AMEntry where
  show (ptr :=> tgt) = show ptr ++ " maps to " ++ show tgt

data AMFrom =
    AMAccount AccountID
  | AMGroup GroupID
  | AMAttributes Attributes
  deriving (Eq)

instance Show AMFrom where
  show (AMAccount i) = "account #" ++ show i
  show (AMGroup i)   = "group #"   ++ show i
  show (AMAttributes as) = "attributes " ++ showA as

data AMTo =
    ToAccountPlan AccountPlan
  | ToAttributes Attributes
  deriving (Eq)

instance Show AMTo where
  show (ToAccountPlan p) = "account plan item:\n" ++ show p
  show (ToAttributes as) = "attributes " ++ showA as

