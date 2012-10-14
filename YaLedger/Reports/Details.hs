{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, TypeFamilies #-}

module YaLedger.Reports.Details
  (Details (..)) where

import YaLedger.Reports.API

data Details = Details

data DOptions = DCSV (Maybe String)
  deriving (Eq)

instance ReportClass Details where
  type Options Details = DOptions
  type Parameters Details = Maybe Path

  reportOptions _ =
    [Option "C" ["csv"] (OptArg DCSV "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)"]

  reportHelp _ = ""

  runReport _ qry options mbPath = 
      details qry options mbPath
    `catchWithSrcLoc`
      (\l (e :: InternalError) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: InvalidPath) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: NoSuchRate) -> handler l e)

details qry options mbPath = do
    coa <- case mbPath of
              Nothing   -> gets lsCoA
              Just path -> getCoAItem (gets lsPosition) (gets lsCoA) path
    let format = case [s | DCSV s <- options] of
                   []    -> showEntries ASCII
                   (x:_) -> showEntries (CSV x)
    forL coa $ \path acc -> do
      entries <- getEntries acc
      res <- saldo qry acc
      wrapIO $ do
        putStrLn $ path ++ ":"
        putStrLn $ format res (reverse $ filter (checkQuery qry) entries)

