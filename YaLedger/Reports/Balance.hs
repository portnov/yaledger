{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.Balance where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Data.Dates

import YaLedger.Types
import YaLedger.Tree
import YaLedger.Kernel
import YaLedger.Monad
import YaLedger.Exceptions

sumGroup :: (Throws InternalError l,
             Throws NoSuchRate l)
         => AccountGroupData
         -> [Amount]
         -> Ledger l Amount
sumGroup ag ams = do
  let c = agCurrency ag
  ams' <- mapM (convert c) ams
  let res = sum [x | x :# _ <- ams']
  return $ res :# c

balance :: (Throws InternalError l,
            Throws NoSuchRate l)
        => Ledger l (Tree NotLinked Amount Amount)
balance = do
  now <- wrapIO $ getCurrentDateTime
  let qry = Query {
             qStart = Nothing,
             qEnd   = Just now,
             qAttributes = [] }
  plan <- gets lsAccountPlan
  mapTreeM sumGroup (saldo qry) plan

