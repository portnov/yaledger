{-# LANGUAGE EmptyDataDecls, GADTs, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeSynonymInstances, DeriveDataTypeable, RecordWildCards, ScopedTypeVariables, MultiParamTypeClasses #-}

module YaLedger.Types
  (Record (..),
   Transaction (..),
   Query (..),
   module YaLedger.Tree,
   module YaLedger.Attributes,
   module YaLedger.Types.Common,
   module YaLedger.Types.Ledger,
   module YaLedger.Types.Map,
   trace
  ) where

import Data.List
import Data.Dates
import Data.Decimal
import Data.IORef
import qualified Data.Map as M
import Text.Printf

import YaLedger.Tree
import YaLedger.Attributes
import YaLedger.Types.Common
import YaLedger.Types.Ledger
import YaLedger.Types.Map

import Debug.Trace

data Record =
    Template String (Transaction Param)
  | Transaction (Transaction Amount)
  deriving (Show)

data Transaction v =
    TEntry (Entry v Unchecked)
  | TReconciliate AnyAccount v
  | TCallTemplate String [Amount]
  | TSetRate Currency Currency Double
  deriving (Eq, Show)

data Query = Query {
    qStart :: Maybe DateTime,
    qEnd   :: Maybe DateTime,
    qAttributes :: Attributes }
  deriving (Eq, Show)

