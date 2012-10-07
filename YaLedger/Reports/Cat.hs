{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.Cat where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc

import YaLedger.Types.Reports
import YaLedger.Kernel
import YaLedger.Monad
import YaLedger.Exceptions
import YaLedger.Logger
import YaLedger.Pretty

data Cat = Cat

instance ReportClass Cat where
  type Options Cat = ()
  type Parameters Cat = ()
  reportOptions _ = []
  defaultOptions _ = []
  reportHelp _ = ""

  runReport _ qry _ () = 
      cat' qry
    `catchWithSrcLoc`
      (\l (e :: InternalError) -> handler l e)

cat' qry = do
  records <- gets lsLoadedRecords
  forM_ (filter (checkQuery qry) records) $ \record ->
      wrapIO $ putStrLn $ prettyPrint record

