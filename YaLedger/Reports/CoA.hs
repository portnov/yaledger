{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.CoA where

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

showCoA :: Query -> Maybe Path -> Ledger NoExceptions ()
showCoA _ mbPath = showCoA' mbPath
  `catchWithSrcLoc`
    (\l (e :: InternalError) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: InvalidPath) -> handler l e)

showCoA' :: (Throws InvalidPath l,
              Throws InternalError l)
          => Maybe Path
          -> Ledger l ()
showCoA' mbPath = do
  coa <- case mbPath of
            Nothing   -> gets lsCoA
            Just path -> getCoAItem (gets lsPosition) (gets lsCoA) path
  wrapIO $ putStrLn $ show coa

