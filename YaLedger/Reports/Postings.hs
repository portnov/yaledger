{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, TypeFamilies #-}

module YaLedger.Reports.Postings where

import YaLedger.Reports.API

data Postings = Postings

instance ReportClass Postings where
  type Options Postings = ()
  type Parameters Postings = Maybe Path
  reportOptions _ = []
  defaultOptions _ = []
  reportHelp _ = ""

  runReport _ qry _ mbPath =
      postings' qry mbPath
    `catchWithSrcLoc`
      (\l (e :: InternalError) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: InvalidPath) -> handler l e)

postings' qry mbPath = do
  coa <- case mbPath of
            Nothing   -> gets lsCoA
            Just path -> getCoAItem (gets lsPosition) (gets lsCoA) path
  forL coa $ \path acc -> do
      credit <- readIOList =<< creditPostings acc
      debit  <- readIOList =<< debitPostings  acc
      let postings = sort $ filter (checkQuery qry) (map left credit ++ map right debit)
          res = unlines $ showPostings postings
      wrapIO $ do
        putStrLn $ path ++ ":"
        putStrLn res

left :: Ext (Posting Decimal Credit) -> Ext (Either (Posting Decimal Credit) (Posting Decimal Debit))
left (Ext date pos attrs posting) = Ext date pos attrs (Left posting)

right :: Ext (Posting Decimal Debit) -> Ext (Either (Posting Decimal Credit) (Posting Decimal Debit))
right (Ext date pos attrs posting) = Ext date pos attrs (Right posting)

showPostings :: [Ext (Either (Posting Decimal Credit) (Posting Decimal Debit))] -> [String]
showPostings [] = ["No postings."]
showPostings list =
    let dates = map (prettyPrint . getDate) list
        amounts = map getAmountS list

        getAmountS (Ext {getContent = (Left p)}) = show (getAmount p)
        getAmountS (Ext {getContent = (Right p)}) = '-': show (getAmount p)
    in  twoColumns "DATE" "AMOUNT" (alignMax ACenter dates) (alignMax ARight amounts)

