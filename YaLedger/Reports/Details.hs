{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, TypeFamilies #-}

module YaLedger.Reports.Details
  (Details (..)) where

import YaLedger.Reports.API

data Details = Details

type DOptions = CommonFlags

instance ReportClass Details where
  type Options Details = DOptions
  type Parameters Details = Maybe Path

  reportOptions _ =
    [Option "C" ["csv"] (OptArg CCSV "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)"]

  reportHelp _ = ""

  initReport _ options _ = setOutputFormat options

  runReport _ qry options mbPath = 
      details qry options mbPath
    `catchWithSrcLoc`
      (\l (e :: InternalError) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: InvalidPath) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: NoSuchRate) -> handler l e)

details qry options mbPath = do
    coa <- getCoAItemL mbPath
    let format = case selectOutputFormat options of
                   OASCII _ -> showEntries ASCII
                   OCSV csv -> showEntries csv
    forL coa $ \path acc -> do
      entries <- getEntries acc
      res <- saldo qry acc
      outputString $ path ++ ":"
      outputText $ format res (reverse $ filter (checkQuery qry) entries)

