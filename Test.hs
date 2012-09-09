
module Test where

import Control.Monad
import Text.Parsec

import YaLedger.Types
import YaLedger.Tree
import YaLedger.Kernel
import YaLedger.Correspondence
import qualified YaLedger.Parser.Plan as Plan
import qualified YaLedger.Parser.Transactions as T
import qualified YaLedger.Parser.Map as Map

readPlan :: FilePath -> IO AccountPlan
readPlan path = do
  content <- readFile path
  case runParser Plan.pAccountGroup Plan.emptyPState path content of
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

test :: IO ()
test = do
  plan <- readPlan "test.accounts"
  print plan
  amap <- readAMap plan "test.map"
  trans <- readTrans plan "test.yaledger"
  forM trans print
  return ()
