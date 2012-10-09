{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies #-}

module YaLedger.Reports.Cat where

import YaLedger.Reports.API

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

