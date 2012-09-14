
module YaLedger.Parser where

import Text.Parsec

import YaLedger.Types
import qualified YaLedger.Parser.Plan as Plan
import qualified YaLedger.Parser.Map  as Map
import qualified YaLedger.Parser.Transactions as T

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

