{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.Balance where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc
import qualified Data.Map as M
import Data.Dates

import YaLedger.Types
import YaLedger.Types.Reports
import YaLedger.Kernel
import YaLedger.Monad
import YaLedger.Exceptions

sumGroup :: (Throws InternalError l,
             Throws NoSuchRate l)
         => AccountGroupData
         -> [Amount]
         -> Ledger l Amount
sumGroup ag ams = do
  setPos $ newPos ("accounts group " ++ agName ag) 0 0
  let c = agCurrency ag
  ams' <- mapM (convert c) ams
  let res = sum [x | x :# _ <- ams']
  return $ res :# c

balance :: Query
        -> Maybe Path
        -> Ledger NoExceptions ()
balance qry mbPath = (do
    plan <- case mbPath of
              Nothing   -> gets lsAccountPlan
              Just path -> getAccountPlanItem path
    res <- mapTreeM sumGroup (saldo qry) plan
    wrapIO $ print res)
  `catchWithSrcLoc`
    (\l (e :: InvalidPath) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: NoSuchRate) -> handler l e)

