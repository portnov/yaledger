{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, TypeFamilies #-}

module YaLedger.Reports.Postings
  (Postings (..)) where

import YaLedger.Reports.API

data Postings = Postings

type POptions = CommonFlags

instance ReportClass Postings where
  type Options Postings = POptions
  type Parameters Postings = Maybe Path

  reportOptions _ =
    [Option "C" ["csv"] (OptArg CCSV "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)",
     Option "H" ["html"] (NoArg CHTML) "Output data in HTML format"]

  reportHelp _ = "Outputs list of postings from one account or accounts group."

  initReport _ options _ = setOutputFormat options

  runReport _ qry options mbPath =
      postings qry options mbPath
    `catchWithSrcLoc`
      (\l (e :: InternalError) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: InvalidPath) -> handler l e)

postings qry options mbPath = do
  coa <- getCoAItemL mbPath
  let format a = case selectOutputFormat options of
                   OASCII _ -> showPostings a ASCII
                   OCSV csv -> showPostings a csv
                   OHTML html -> showPostings a html
  cfg <- gets lsConfig
  forL coa $ \path acc -> do
      let assets = isAssets cfg (getAttrs acc)
      credit <- readIOListL =<< creditPostings acc
      debit  <- readIOListL =<< debitPostings  acc
      let postings = sort $ filter (checkQuery qry) (map left credit ++ map right debit)
          res = unlinesText $ format assets postings
      outputString $ path ++ ":"
      outputText res

left :: Ext (Posting Decimal Credit) -> Ext (Either (Posting Decimal Credit) (Posting Decimal Debit))
left (Ext date i pos attrs posting) = Ext date i pos attrs (Left posting)

right :: Ext (Posting Decimal Debit) -> Ext (Either (Posting Decimal Credit) (Posting Decimal Debit))
right (Ext date i pos attrs posting) = Ext date i pos attrs (Right posting)

showPostings :: TableFormat a => Bool -> a -> [Ext (Either (Posting Decimal Credit) (Posting Decimal Debit))] -> [FormattedText]
showPostings _ _ [] = [output "No postings."]
showPostings assets f list =
    let dates = map (prettyPrint . getDate) list
        amounts = map getAmountS list

        getAmountS (Ext {getContent = (Left p)})
          | assets = "-" <> prettyPrint (getAmount p)
          | otherwise = prettyPrint (getAmount p)
        getAmountS (Ext {getContent = (Right p)})
          | assets = prettyPrint (getAmount p)
          | otherwise = "-" <> prettyPrint (getAmount p)
    in  tableColumns f [([output "DATE"], ALeft, dates), ([output "AMOUNT"], ARight, amounts)]

