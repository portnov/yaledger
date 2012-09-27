
module YaLedger.Parser where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Maybe
import Data.List
import Text.Parsec
import System.FilePath
import System.FilePath.Glob

import YaLedger.Types
import qualified YaLedger.Parser.CoA as CoA
import qualified YaLedger.Parser.Map  as Map
import qualified YaLedger.Parser.Transactions as T
import qualified YaLedger.Parser.CSV as CSV

type InputParser = FilePath -> ChartOfAccounts -> FilePath -> IO [Ext Record]

allParsers :: [(String, String, InputParser)]
allParsers =
  [("yaledger", "*.yaledger", readTrans),
   ("csv",      "*.csv",      CSV.loadCSV)]

lookupMask :: FilePath -> [(String, String, InputParser)] -> Maybe (String, InputParser)
lookupMask _ [] = Nothing
lookupMask file ((name,mask,parser):xs)
  | match (compile mask) file = Just (name, parser)
  | otherwise                 = lookupMask file xs

parseInputFiles :: [(String, FilePath)] -> ChartOfAccounts -> [FilePath] -> IO [Ext Record]
parseInputFiles configs coa masks = do
  inputFiles <- concat <$> mapM glob masks
  records <- forM inputFiles $ \file -> do
                 case lookupMask (takeFileName file) allParsers of
                   Nothing -> fail $ "Unknown file type: " ++ file
                   Just (parserName, parser) ->
                     let configFile = fromMaybe (parserName ++ ".yaml") $
                                          lookup parserName configs
                     in  parser configFile coa file
  return $ sort $ concat records

readCoA :: FilePath -> IO ChartOfAccounts
readCoA path = do
  content <- readFile path
  res <- runParserT CoA.pAccountGroup CoA.emptyPState path content
  case res of
    Right res -> return res
    Left err -> fail $ show err

readAMap :: ChartOfAccounts -> FilePath -> IO AccountMap
readAMap coa path = do
  content <- readFile path
  case runParser Map.pAccountMap (Map.PState coa) path content of
    Right res -> return res
    Left err -> fail $ show err

readTrans :: FilePath -> ChartOfAccounts -> FilePath -> IO [Ext Record]
readTrans _ coa path = do
  content <- readFile path
  st <- T.emptyPState coa
  case runParser T.pRecords st path content of
    Right res -> return res
    Left err -> fail $ show err

