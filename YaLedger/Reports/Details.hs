{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverlappingInstances, GADTs #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.Details where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc
import Data.Maybe
import qualified Data.Map as M
import Data.Dates
import Data.Decimal
import Text.Printf

import YaLedger.Types
import YaLedger.Types.Reports
import YaLedger.Kernel
import YaLedger.Monad
import YaLedger.Exceptions
import YaLedger.Logger
import YaLedger.Reports.Common

details :: Query -> Maybe Path -> Ledger NoExceptions ()
details qry mbPath =
    details' qry mbPath
  `catchWithSrcLoc`
    (\l (e :: InternalError) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: InvalidPath) -> handler l e)
  `catchWithSrcLoc`
    (\l (e :: NoSuchRate) -> handler l e)

details' qry mbPath = do
    coa <- case mbPath of
              Nothing   -> gets lsCoA
              Just path -> getCoAItem (gets lsPosition) (gets lsCoA) path
    forL coa $ \path acc -> do
      entries <- getEntries acc
      res <- saldo qry acc
      wrapIO $ do
        putStrLn $ path ++ ":"
        putStrLn $ showEntries res (reverse entries)

