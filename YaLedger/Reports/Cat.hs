{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies, RecordWildCards, GADTs #-}

module YaLedger.Reports.Cat
  (Cat (..)) where

import YaLedger.Reports.API

data Cat = Cat

data COptions = CCSV (Maybe String)

instance ReportClass Cat where
  type Options Cat = COptions
  type Parameters Cat = ()

  reportOptions _ = 
    [ Option "C" ["csv"] (OptArg CCSV "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default). Only entries are ouput."]

  reportHelp _ = "Outputs all loaded records (after deduplication, if any)."

  runReport _ qry options () = 
      case [s | CCSV s <- options] of
        [] -> cat qry
        (s:_) -> catCSV s qry
    `catchWithSrcLoc`
      (\l (e :: InternalError) -> handler l e)

cat qry = do
  records <- gets lsLoadedRecords
  forM_ (filter (checkQuery qry) records) $ \record ->
      wrapIO $ putStrLn $ prettyPrint record

csvRecord :: ChartOfAccounts -> Ext Record -> Maybe Row
csvRecord coa (Ext {getDate=date, getContent=rec}) = go date rec
  where
    go date (Transaction (TEntry (UEntry {..}))) = Just $
      [[prettyPrint date],
       map (showPostingAccount coa) uEntryCreditPostings,
       map showPostingValue         uEntryCreditPostings,
       map (showPostingAccount coa) uEntryDebitPostings,
       map showPostingValue         uEntryDebitPostings ]
    go _ t = Nothing

catCSV sep qry = do
  coa <- gets lsCoA
  allRecords <- gets lsLoadedRecords
  let records = filter (checkQuery qry) allRecords
      rows = mapMaybe (csvRecord coa) allRecords
  wrapIO $ putStrLn $ unlines $
           tableGrid (CSV sep) [(ALeft, ["DATE"]),
                                (ALeft, ["CREDIT ACCOUNT"]),
                                (ALeft, ["CREDIT AMOUNT"]),
                                (ALeft, ["DEBIT ACCOUNT"]),
                                (ALeft, ["DEBIT AMOUNT"])] rows

