{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.Cat where

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
import YaLedger.Pretty
import YaLedger.Reports.Common

cat :: Query -> Ledger NoExceptions ()
cat qry =
    cat' qry
  `catchWithSrcLoc`
    (\l (e :: InternalError) -> handler l e)

cat' qry = do
  records <- gets lsLoadedRecords
  forM_ (filter (checkQuery qry) records) $ \record ->
      wrapIO $ putStrLn $ prettyPrint record

