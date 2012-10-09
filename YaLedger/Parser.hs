-- | This module contains high-level functions for parsing input files
module YaLedger.Parser
  (InputParser,
   allParsers,
   parseInputFiles,
   readCoA,
   readAMap
  ) where

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

-- | All supported parsers
allParsers :: [(String, String, InputParser)]
allParsers =
  [("yaledger", "*.yaledger", T.loadTransactions),
   ("csv",      "*.csv",      CSV.loadCSV),
   ("html",     "*.html",     HTML.loadHTML),
   ("cbr",      "*.cbr",      CBR.loadCBR)]

-- | Lookup for parser by file mask
lookupMask :: FilePath -> [(String, String, InputParser)] -> Maybe (String, InputParser)
lookupMask _ [] = Nothing
lookupMask file ((name,mask,parser):xs)
  | match (compile mask) file = Just (name, parser)
  | otherwise                 = lookupMask file xs

-- | Parse all input files using corresponding parsers
parseInputFiles :: [(String, String, InputParser)] -- ^ List of parsers: (name, files mask, parser)
                -> [(String, FilePath)]            -- ^ (parser name, config path)
                -> Currencies                      -- ^ Set of declared currencies
                -> ChartOfAccounts
                -> [FilePath]                      -- ^ List of files or masks (*.yaledger)
                -> IO [Ext Record]
parseInputFiles parsers configs currs coa masks = do
  inputFiles <- concat <$> mapM glob masks
  infoM rootLoggerName $ "INFO: Input files:\n" ++ unlines inputFiles
  records <- forM inputFiles $ \file -> do
                 case lookupMask (takeFileName file) parsers of
                   Nothing -> fail $ "Unknown file type: " ++ file
                   Just (parserName, parser) ->
                     let configFile = fromMaybe (parserName ++ ".yaml") $
                                          lookup parserName configs
                     in  do
                         rec <- parser configFile currs coa file
                         infoM rootLoggerName $ "INFO: Read " ++ show (length rec) ++ " records from " ++ file
                         return rec
  return $ sort $ concat records

-- | Read chart of accounts from file
readCoA :: Currencies -> FilePath -> IO ChartOfAccounts
readCoA currs path = do
  content <- readFile path
  res <- runParserT CoA.pAccountGroup (CoA.emptyPState currs) path content
  case res of
    Right res -> return res
    Left err -> fail $ show err

-- | Read accounts map from file
readAMap :: ChartOfAccounts -> FilePath -> IO AccountMap
readAMap coa path = do
  content <- readFile path
  case runParser Map.pAccountMap (Map.PState coa) path content of
    Right res -> return res
    Left err -> fail $ show err

