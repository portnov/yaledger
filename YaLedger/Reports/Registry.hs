{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.Registry where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc
import Data.List
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

registry :: Query -> Maybe Path -> Ledger NoExceptions ()
registry qry mbPath =
    registry' qry mbPath
  `catchWithSrcLoc`
    (\l (e :: InternalError) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: InvalidPath) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: NoSuchRate) -> handler l e)

registry' qry mbPath = do
    plan <- case mbPath of
              Nothing   -> gets lsAccountPlan
              Just path -> getAccountPlanItem path
    let accounts = map snd $ leafs plan
    allEntries <- forM accounts $ \acc ->
                      readIOList (accountEntries acc)
    totals <- do
              res <- treeSaldo qry plan
              case res of
                Leaf {..}   -> return leafData
                Branch {..} -> return branchData
    wrapIO $ putStrLn $ showEntries totals (nub $ sort $ concat allEntries)
  
