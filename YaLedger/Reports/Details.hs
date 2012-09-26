{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.Details where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc
import qualified Data.Map as M
import Data.Dates
import Data.Decimal
import Text.Printf

import YaLedger.Types
import YaLedger.Types.Reports
import YaLedger.Kernel
import YaLedger.Monad
import YaLedger.Exceptions
import YaLedger.Reports.Common

details :: Query -> Maybe Path -> Ledger NoExceptions ()
details qry mbPath =
    details' qry mbPath
  `catchWithSrcLoc`
    (\l (e :: InternalError) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: InvalidPath) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: NoSuchRate) -> handler l e)

details' qry mbPath = do
    plan <- case mbPath of
              Nothing   -> gets lsAccountPlan
              Just path -> getAccountPlanItem (gets lsPosition) (gets lsAccountPlan) path
    forL plan $ \path acc -> do
      entries <- readIOList (accountEntries acc)
      res <- saldo qry acc
      wrapIO $ do
        putStrLn $ path ++ ":"
        putStrLn $ showEntries res (reverse entries)

