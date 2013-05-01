{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies #-}

module YaLedger.Reports.API
  ( -- * Re-export generic Control.* modules
   module Control.Applicative,
   module Control.Monad,
   module Control.Monad.State,
   module Control.Monad.Exception,
   module Control.Monad.Loc,

   -- * Re-export generic Data.* modules
   module Data.Maybe,
   module Data.List,
   module Data.Decimal,

   -- * Re-export YaLedger kernel modules
   module YaLedger.Types,
   module YaLedger.Types.Monad,
   module YaLedger.Exceptions,
   module YaLedger.Types.Reports,
   module YaLedger.Kernel,
   module YaLedger.Kernel.STM,
   module YaLedger.Logger,
   module YaLedger.Reports.Common,
   module YaLedger.Reports.Ledger,
   module YaLedger.Output,
   module YaLedger.Output.ANSI,

   getCoAItemL
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Data.Maybe
import Data.List
import Data.Decimal

import YaLedger.Types
import YaLedger.Types.Monad
import YaLedger.Types.Reports
import YaLedger.Kernel
import YaLedger.Kernel.STM
import YaLedger.Exceptions
import YaLedger.Logger
import YaLedger.Reports.Common
import YaLedger.Reports.Ledger
import YaLedger.Output
import YaLedger.Output.ANSI

getCoAItemL :: Throws InvalidPath l
            => Maybe Path
            -> Ledger l ChartOfAccounts
getCoAItemL Nothing = gets lsCoA
getCoAItemL (Just path) = getCoAItem path

