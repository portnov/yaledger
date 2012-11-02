{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, TypeFamilies #-}

module YaLedger.Reports.Postings
  (Postings (..)) where

import YaLedger.Reports.API

data Postings = Postings

data POptions = PCSV (Maybe String)

instance ReportClass Postings where
  type Options Postings = POptions
  type Parameters Postings = Maybe Path

  reportOptions _ =
    [Option "C" ["csv"] (OptArg PCSV "SEPARATOR") "Output data in CSV format using given fields delimiter (semicolon by default)"]

  reportHelp _ = "Outputs list of postings from one account or accounts group."

  runReport _ qry options mbPath =
      postings qry options mbPath
    `catchWithSrcLoc`
      (\l (e :: InternalError) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: InvalidPath) -> handler l e)

postings qry options mbPath = do
  coa <- case mbPath of
            Nothing   -> gets lsCoA
            Just path -> getCoAItem (gets lsPosition) (gets lsCoA) path
  let format = case [s | PCSV s <- options] of
                 []    -> showPostings ASCII
                 (x:_) -> showPostings (CSV x)
  forL coa $ \path acc -> do
      credit <- readIOList =<< creditPostings acc
      debit  <- readIOList =<< debitPostings  acc
      let postings = sort $ filter (checkQuery qry) (map left credit ++ map right debit)
          res = unlines $ format postings
      wrapIO $ do
        putStrLn $ path ++ ":"
        putStrLn res

left :: Ext (Posting Decimal Credit) -> Ext (Either (Posting Decimal Credit) (Posting Decimal Debit))
left (Ext date i pos attrs posting) = Ext date i pos attrs (Left posting)

right :: Ext (Posting Decimal Debit) -> Ext (Either (Posting Decimal Credit) (Posting Decimal Debit))
right (Ext date i pos attrs posting) = Ext date i pos attrs (Right posting)

showPostings :: TableFormat a => a -> [Ext (Either (Posting Decimal Credit) (Posting Decimal Debit))] -> [String]
showPostings _ [] = ["No postings."]
showPostings f list =
    let dates = map (prettyPrint . getDate) list
        amounts = map getAmountS list

        getAmountS (Ext {getContent = (Left p)}) = show (getAmount p)
        getAmountS (Ext {getContent = (Right p)}) = '-': show (getAmount p)
    in  tableColumns f [(["DATE"], ALeft, dates), (["AMOUNT"], ARight, amounts)]

