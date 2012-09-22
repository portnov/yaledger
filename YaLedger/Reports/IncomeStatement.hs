{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.IncomeStatement where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc
import qualified Data.Map as M
import Data.List
import Data.Dates

import YaLedger.Types
import YaLedger.Types.Reports
import YaLedger.Strings
import YaLedger.Kernel
import YaLedger.Monad
import YaLedger.Exceptions
import YaLedger.Reports.Common

incomeStatement :: Query
                -> Maybe Path
                -> Ledger NoExceptions ()
incomeStatement qry mbPath =
    incomeStatement' qry mbPath
  `catchWithSrcLoc`
    (\l (e :: InternalError) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: InvalidPath) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: NoSuchRate) -> handler l e)

incomeStatement' qry mbPath = do
    plan <- case mbPath of
              Nothing   -> gets lsAccountPlan
              Just path -> getAccountPlanItem path
    let isCredit (WCredit _ _) = True
        isCredit _             = False

        isDebit (WDebit _ _) = True
        isDebit _            = False

        amount (Branch {..}) = branchData
        amount (Leaf   {..}) = leafData

        incomes  = filterLeafs isDebit  plan
        outcomes = filterLeafs isCredit plan

    incomes'  <- mapTree negateAmount negateAmount <$> treeSaldo qry incomes
    outcomes' <- treeSaldo qry outcomes

    defcur <- gets lsDefaultCurrency
    incomeD  :# _ <- convert defcur (amount incomes')
    outcomeD :# _ <- convert defcur (amount outcomes')

    let incomesS  = lines (show incomes')
        outcomesS = lines (show outcomes')
        m = max (length incomesS) (length outcomesS)
        padE list = list ++ replicate (m - length list) ""
        res = twoColumns "INCOMES" "OUTCOMES"
                 (alignMax ALeft $ padE incomesS)
                 (alignMax ALeft $ padE outcomesS)
        sep = replicate (length $ head res) '='
        footer = "    TOTALS: " ++ show (incomeD - outcomeD) ++ defcur

    wrapIO $ putStrLn $ unlines (res ++ [sep, footer])
      
