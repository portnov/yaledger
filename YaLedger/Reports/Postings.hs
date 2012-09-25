{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.Postings where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc
import qualified Data.Map as M
import Data.List
import Data.Decimal
import Data.Dates

import YaLedger.Types
import YaLedger.Types.Reports
import YaLedger.Kernel
import YaLedger.Monad
import YaLedger.Strings
import YaLedger.Pretty
import YaLedger.Exceptions
import YaLedger.Reports.Common

postings :: Query -> AnyAccount -> Ledger NoExceptions ()
postings _ acc = do
    postings' acc
  `catchWithSrcLoc`
    (\l (e :: InternalError) -> handler l e)

postings' acc = do
  credit <- readIOList =<< creditPostings acc
  debit  <- readIOList =<< debitPostings  acc
  let postings = sort (map left credit ++ map right debit)
      res = unlines $ showPostings postings
  wrapIO $ putStrLn res

left :: Ext (Posting Decimal Credit) -> Ext (Either (Posting Decimal Credit) (Posting Decimal Debit))
left (Ext date pos attrs posting) = Ext date pos attrs (Left posting)

right :: Ext (Posting Decimal Debit) -> Ext (Either (Posting Decimal Credit) (Posting Decimal Debit))
right (Ext date pos attrs posting) = Ext date pos attrs (Right posting)

showPostings :: [Ext (Either (Posting Decimal Credit) (Posting Decimal Debit))] -> [String]
showPostings list =
    let dates = map (prettyPrint . getDate) list
        amounts = map getAmountS list

        getAmountS (Ext {getContent = (Left p)}) = show (getAmount p)
        getAmountS (Ext {getContent = (Right p)}) = '-': show (getAmount p)
    in  twoColumns "DATE" "AMOUNT" (alignMax ACenter dates) (alignMax ARight amounts)
