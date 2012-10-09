{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, TypeFamilies #-}

module YaLedger.Reports.Details where

import YaLedger.Reports.API

data Details = Details

instance ReportClass Details where
  type Options Details = ()
  type Parameters Details = Maybe Path
  reportOptions _ = []
  defaultOptions _ = []
  reportHelp _ = ""

  runReport _ qry _ mbPath = 
      details' qry mbPath
    `catchWithSrcLoc`
      (\l (e :: InternalError) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: InvalidPath) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: NoSuchRate) -> handler l e)

details' qry mbPath = do
    coa <- case mbPath of
              Nothing   -> gets lsCoA
              Just path -> getCoAItem (gets lsPosition) (gets lsCoA) path
    forL coa $ \path acc -> do
      entries <- getEntries acc
      res <- saldo qry acc
      wrapIO $ do
        putStrLn $ path ++ ":"
        putStrLn $ showEntries res (reverse $ filter (checkQuery qry) entries)

