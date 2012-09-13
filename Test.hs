{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module Test where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc
import Data.Dates
import Text.Parsec

import YaLedger.Types
import YaLedger.Tree
import YaLedger.Kernel
import YaLedger.Correspondence
import YaLedger.Processor
import YaLedger.Exceptions
import YaLedger.Monad
import qualified YaLedger.Parser.Plan as Plan
import qualified YaLedger.Parser.Transactions as T
import qualified YaLedger.Parser.Map as Map

readPlan :: FilePath -> IO AccountPlan
readPlan path = do
  content <- readFile path
  res <- runParserT Plan.pAccountGroup Plan.emptyPState path content
  case res of
    Right res -> return res
    Left err -> fail $ show err

readAMap :: AccountPlan -> FilePath -> IO AccountMap
readAMap plan path = do
  content <- readFile path
  case runParser Map.pAccountMap (Map.PState plan) path content of
    Right res -> return res
    Left err -> fail $ show err

readTrans :: AccountPlan -> FilePath -> IO [Ext Record]
readTrans plan path = do
  content <- readFile path
  st <- T.emptyPState plan
  case runParser T.pRecords st path content of
    Right res -> return res
    Left err -> fail $ show err

balance :: (Throws InternalError l,
            Throws NoSuchRate l)
        => Ledger l (Tree NotLinked AccountGroupData Amount)
balance = do
  now <- wrapIO $ getCurrentDateTime
  let qry = Query {
             qStart = Nothing,
             qEnd   = Just now,
             qAttributes = [] }
  plan <- gets lsAccountPlan
  mapLeafsM (saldo qry) plan

process :: [Ext Record] -> LedgerMonad ()
process trans =
  runEMT $ do
           forM_ trans processTransaction
           b <- balance
           wrapIO $ print b
        `catchWithSrcLoc`
           (\loc (e :: InvalidAccountType) -> wrapIO $ putStrLn (showExceptionWithTrace loc e))
        `catchWithSrcLoc`
           (\loc (e :: NoSuchRate) -> wrapIO $ putStrLn (showExceptionWithTrace loc e))
        `catchWithSrcLoc`
           (\loc (e :: NoCorrespondingAccountFound) -> wrapIO $ putStrLn (showExceptionWithTrace loc e))
        `catchWithSrcLoc`
           (\loc (e :: NoSuchTemplate) -> wrapIO $ putStrLn (showExceptionWithTrace loc e))

test :: IO ()
test = do
  plan <- readPlan "test.accounts"
  print plan
  amap <- readAMap plan "test.map"
  forM amap print
  trans <- readTrans plan "test.yaledger"
  forM trans print
  runLedger plan amap $ process trans
  return ()
