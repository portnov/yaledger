
module YaLedger.Parser where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Maybe
import Data.List
import Text.Parsec
import System.FilePath
import System.FilePath.Glob
import System.Log.Logger

import YaLedger.Types
import qualified YaLedger.Parser.CoA as CoA
import qualified YaLedger.Parser.Map  as Map
import qualified YaLedger.Parser.Transactions as T
import qualified YaLedger.Parser.CSV as CSV
import qualified YaLedger.Parser.HTML as HTML
import qualified YaLedger.Parser.CBR as CBR

type InputParser = FilePath -> Currencies -> ChartOfAccounts -> FilePath -> IO [Ext Record]

allParsers :: [(String, String, InputParser)]
allParsers =
  [("yaledger", "*.yaledger", readTrans),
   ("csv",      "*.csv",      CSV.loadCSV),
   ("html",     "*.html",     HTML.loadHTML),
   ("cbr",      "*.cbr",      CBR.loadCBR)]

lookupMask :: FilePath -> [(String, String, InputParser)] -> Maybe (String, InputParser)
lookupMask _ [] = Nothing
lookupMask file ((name,mask,parser):xs)
  | match (compile mask) file = Just (name, parser)
  | otherwise                 = lookupMask file xs

parseInputFiles :: [(String, FilePath)] -> Currencies -> ChartOfAccounts -> [FilePath] -> IO [Ext Record]
parseInputFiles configs currs coa masks = do
  inputFiles <- concat <$> mapM glob masks
  infoM rootLoggerName $ "Input files:\n" ++ unlines inputFiles
  records <- forM inputFiles $ \file -> do
                 case lookupMask (takeFileName file) allParsers of
                   Nothing -> fail $ "Unknown file type: " ++ file
                   Just (parserName, parser) ->
                     let configFile = fromMaybe (parserName ++ ".yaml") $
                                          lookup parserName configs
                     in  do
                         rec <- parser configFile currs coa file
                         infoM rootLoggerName $ "Read " ++ show (length rec) ++ " records from " ++ file
                         return rec
  return $ sort $ concat records

readCoA :: Currencies -> FilePath -> IO ChartOfAccounts
readCoA currs path = do
  content <- readFile path
  res <- runParserT CoA.pAccountGroup (CoA.emptyPState currs) path content
  case res of
    Right res -> return res
    Left err -> fail $ show err

readAMap :: ChartOfAccounts -> FilePath -> IO AccountMap
readAMap coa path = do
  content <- readFile path
  case runParser Map.pAccountMap (Map.PState coa) path content of
    Right res -> return res
    Left err -> fail $ show err

readTrans :: FilePath -> Currencies -> ChartOfAccounts -> FilePath -> IO [Ext Record]
readTrans _ currs coa path = do
  content <- readFile path
  st <- T.emptyPState coa currs
  case runParser T.pRecords st path content of
    Right res -> return res
    Left err -> fail $ show err

