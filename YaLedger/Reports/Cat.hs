{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, TypeFamilies, RecordWildCards, GADTs #-}

module YaLedger.Reports.Cat
  (Cat (..)) where

import YaLedger.Reports.API

data Cat = Cat

type COptions = CommonFlags

instance ReportClass Cat where
  type Options Cat = COptions
  type Parameters Cat = ()

  reportOptions _ = 
    [ Option "C" ["csv"] (OptArg CCSV "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default). Only entries are ouput."]

  reportHelp _ = "Outputs all loaded records (after deduplication, if any)."

  initReport _ options _ = setOutputFormat options

  runReport _ qry options () = 
      case selectOutputFormat options of
        OASCII _ -> cat qry
        OCSV (CSV s) -> catCSV s qry
    `catchWithSrcLoc`
      (\l (e :: InternalError) -> handler l e)

cat qry = do
  records <- gets lsLoadedRecords
  forM_ (filter (checkQuery qry) records) $ \record ->
      outputText $ prettyPrint record

csvRecord :: ChartOfAccounts -> Ext Record -> Maybe Row
csvRecord coa (Ext {getDate=date, getContent=rec}) = go date rec
  where
    go date (Transaction (TEntry (UEntry {..}))) = Just $
      [[prettyPrint date],
       map (showPostingAccount Nothing coa) uEntryCreditPostings,
       map showPostingValue                 uEntryCreditPostings,
       map (showPostingAccount Nothing coa) uEntryDebitPostings,
       map showPostingValue                 uEntryDebitPostings ]
    go _ t = Nothing

catCSV sep qry = do
  coa <- gets lsCoA
  allRecords <- gets lsLoadedRecords
  let records = filter (checkQuery qry) allRecords
      rows = mapMaybe (csvRecord coa) allRecords
  outputString $ toString $ unlinesText $
           tableGrid (CSV sep) [(ALeft, [output "DATE"]),
                                (ALeft, [output "CREDIT ACCOUNT"]),
                                (ALeft, [output "CREDIT AMOUNT"]),
                                (ALeft, [output "DEBIT ACCOUNT"]),
                                (ALeft, [output "DEBIT AMOUNT"])] rows

