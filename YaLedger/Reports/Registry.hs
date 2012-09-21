{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.Registry where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc
import qualified Data.Map as M
import Data.Dates
import Data.Decimal
import Text.Printf

import YaLedger.Types
import YaLedger.Types.Reports
import YaLedger.Kernel
import YaLedger.Monad
import YaLedger.Exceptions

showE :: Ext (Entry Decimal Checked) -> [String]
showE (Ext {getContent = (CEntry cr dt rd)}) = zipWith go cr' dt' ++ rdS
  where
    rdS
      | rd == OneCurrency = []
      | otherwise = [printf "    Rates difference: %s" (show rd)]
    m = max (length cr) (length dt)
    cr' = take m $ map Just cr ++ repeat Nothing
    dt' = take m $ map Just dt ++ repeat Nothing
    go p1 p2 = printf " %s\t| %s\t |" (posting p1) (posting p2)
    posting Nothing = " "
    posting (Just (DPosting acc x)) = getName acc ++ " " ++ show x
    posting (Just (CPosting acc x)) = getName acc ++ " " ++ show x

showEntries :: Amount -> [Ext (Entry Decimal Checked)] -> String
showEntries totals list =
  let header = "          CREDIT\t|          DEBIT\t |"
      sep    = replicate 50 '='
      lines  = concatMap showE list
      footer = "    TOTALS: " ++ show totals
  in unlines (header:sep:lines ++ [sep,footer])

registry :: Query -> Maybe Path -> Ledger NoExceptions ()
registry qry mbPath =
    registry' qry mbPath
  `catchWithSrcLoc`
    (\l (e :: InternalError) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: InvalidPath) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: NoSuchRate) -> handler l e)

registry' qry mbPath = do
    plan <- case mbPath of
              Nothing   -> gets lsAccountPlan
              Just path -> getAccountPlanItem path
    forL plan $ \path acc -> do
      entries <- readIOList (accountEntries acc)
      res <- saldo qry acc
      wrapIO $ do
        putStrLn $ path ++ ":"
        putStrLn $ showEntries res entries
        putStrLn ""
  
