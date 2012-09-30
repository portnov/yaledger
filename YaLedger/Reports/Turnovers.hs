{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, RecordWildCards #-}
{- # OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.Turnovers where

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
import YaLedger.Logger
import YaLedger.Reports.Common

turnovers :: Query
          -> Maybe Path
          -> Ledger NoExceptions ()
turnovers qry mbPath =
    turnovers' qry mbPath
  `catchWithSrcLoc`
    (\l (e :: InternalError) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: InvalidPath) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: NoSuchRate) -> handler l e)

sumTurnovers ag list = do
  let c = agCurrency ag
  list' <- forM list $ \(cr,dt,t) -> do
                cr' :# _ <- convert c cr
                dt' :# _ <- convert c dt
                t'  :# _ <- convert c t
                return (cr', dt', t')
  let credits = sum $ map (\(x,_,_) -> x) list'
      debits  = sum $ map (\(_,x,_) -> x) list'
      totals  = sum $ map (\(_,_,x) -> x) list'
  return (credits :# c, debits :# c, totals :# c)

allTurnovers qry account = do
  cr :# c <- creditTurnovers qry account
  dt :# _ <- debitTurnovers  qry account
  return (cr :# c, dt :# c, (cr + dt) :# c)

turnovers' qry mbPath = do
    coa <- case mbPath of
              Nothing   -> gets lsCoA
              Just path -> getCoAItem (gets lsPosition) (gets lsCoA) path
    tree <- mapTreeM sumTurnovers (allTurnovers qry) coa
    let struct = showTreeStructure tree
        nodes = allNodes tree
        credits = map (\(x,_,_) -> x) nodes
        debits  = map (\(_,x,_) -> x) nodes
        totals  = map (\(_,_,x) -> x) nodes
    wrapIO $ putStrLn $ unlines $
      columns [("ACCOUNT",alignMax ALeft struct),
               ("CREDIT", alignMax ALeft $ map show credits),
               ("DEBIT",  alignMax ALeft $ map show debits),
               ("TOTALS", alignMax ALeft $ map show totals)]
                  
