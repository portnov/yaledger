{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.IncomeStatement where

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
    let accounts = leafs plan

        isCredit (WCredit _ _) = True
        isCredit _             = False

        isDebit (WDebit _ _) = True
        isDebit _            = False

        incomes  = filter (isDebit  . snd) accounts
        outcomes = filter (isCredit . snd) accounts

    incomes'  <- mapM (saldo qry) $ map snd incomes
    outcomes' <- mapM (saldo qry) $ map snd outcomes
    defcur <- gets lsDefaultCurrency
    incomesD  <- mapM (convert defcur) incomes'
    outcomesD <- mapM (convert defcur) outcomes'

    let go f (path,acc) (x :# _) =
            intercalate "/" (tail $ reverse path) ++
            ": " ++ show (f x) ++ getCurrency acc
        incomesS  = zipWith (go negate) incomes  incomes'
        outcomesS = zipWith (go id)     outcomes outcomes'
        incomeT  = sum [-x | x :# _ <- incomesD]
        outcomeT = sum [x | x :# _ <- outcomesD]
        incomeTS  = "TOTALS: " ++
            show incomeT  ++ defcur
        outcomeTS = "TOTALS: " ++
            show outcomeT ++ defcur
        m = max (length incomes) (length outcomes)
        padE list = list ++ replicate (m - length list) ""

    wrapIO $ putStrLn $ unlines $
      twoColumns "INCOMES" "OUTCOMES"
                 (alignMax ALeft $ padE incomesS ++ [incomeTS])
                 (alignMax ALeft $ padE outcomesS ++ [outcomeTS])
      ++ ["    TOTALS: " ++ show (incomeT - outcomeT) ++ defcur]

