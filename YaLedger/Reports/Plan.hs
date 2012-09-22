{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.Plan where

import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc

import YaLedger.Types
import YaLedger.Types.Reports
import YaLedger.Strings
import YaLedger.Kernel
import YaLedger.Monad
import YaLedger.Exceptions
import YaLedger.Reports.Common

showPlan :: Query -> Maybe Path -> Ledger NoExceptions ()
showPlan _ mbPath = showPlan' mbPath
  `catchWithSrcLoc`
    (\l (e :: InternalError) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: InvalidPath) -> handler l e)

showPlan' :: (Throws InvalidPath l,
              Throws InternalError l)
          => Maybe Path
          -> Ledger l ()
showPlan' mbPath = do
  plan <- case mbPath of
            Nothing   -> gets lsAccountPlan
            Just path -> getAccountPlanItem path
  wrapIO $ putStrLn $ show plan
