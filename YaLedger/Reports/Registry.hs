{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, RecordWildCards, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.Registry where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Data.List

import YaLedger.Types
import YaLedger.Types.Reports
import YaLedger.Kernel
import YaLedger.Monad
import YaLedger.Exceptions
import YaLedger.Logger
import YaLedger.Reports.Common

data Registry = Registry

instance ReportClass Registry where
  type Options Registry = ()
  type Parameters Registry = Maybe Path
  reportOptions _ = []
  defaultOptions _ = []
  reportHelp _ = ""

  runReport _ qry _ mbPath = 
      registry' qry mbPath
    `catchWithSrcLoc`
      (\l (e :: InternalError) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: InvalidPath) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: NoSuchRate) -> handler l e)

registry' qry mbPath = do
    coa <- case mbPath of
              Nothing   -> gets lsCoA
              Just path -> getCoAItem (gets lsPosition) (gets lsCoA) path
    totals <- do
              res <- treeSaldo qry coa
              case res of
                Leaf {..}   -> return leafData
                Branch {..} -> return branchData
    case coa of
      Leaf {leafData = account} -> do
          balances <- readIOList (accountBalances account)
          let balances' = filter (checkQuery qry) balances
          wrapIO $ putStrLn $ showEntriesBalances totals (nub $ sort balances')
      Branch {} -> do
          let accounts = map snd $ leafs coa
          allEntries <- forM accounts getEntries
          let entries = concat $ map (filter $ checkQuery qry) allEntries
          wrapIO $ putStrLn $ showEntries totals (nub $ sort entries)
  
