{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs, RecordWildCards, TypeFamilies #-}

module YaLedger.Reports.CoA where

import YaLedger.Reports.API

data CoA = CoA

instance ReportClass CoA where
  type Options CoA = ()
  type Parameters CoA = Maybe Path
  reportOptions _ = []
  defaultOptions _ = []
  reportHelp _ = ""

  runReport _ qry _ mbPath = 
      showCoA' mbPath
    `catchWithSrcLoc`
      (\l (e :: InternalError) -> handler l e)
    `catchWithSrcLoc`
      (\l (e :: InvalidPath) -> handler l e)

showCoA' :: (Throws InvalidPath l,
              Throws InternalError l)
          => Maybe Path
          -> Ledger l ()
showCoA' mbPath = do
  coa <- case mbPath of
            Nothing   -> gets lsCoA
            Just path -> getCoAItem (gets lsPosition) (gets lsCoA) path
  wrapIO $ putStrLn $ show coa

