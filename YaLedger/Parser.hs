{-# LANGUAGE TemplateHaskell #-}
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
import Control.Concurrent.ParallelIO
import Data.Maybe
import Data.List
import Text.Parsec
import System.Directory
import System.FilePath
import System.FilePath.Glob

import YaLedger.Types
import YaLedger.Logger
import qualified YaLedger.Parser.CoA as CoA
import qualified YaLedger.Parser.Map  as Map
import qualified YaLedger.Parser.Transactions as T
import qualified YaLedger.Parser.CSV as CSV
import qualified YaLedger.Parser.HTML as HTML
import qualified YaLedger.Parser.CBR as CBR

type InputParser = LedgerOptions -> FilePath -> Currencies -> ChartOfAccounts -> FilePath -> IO [Ext Record]

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
parseInputFiles :: LedgerOptions
                -> [(String, String, InputParser)] -- ^ List of parsers: (name, files mask, parser)
                -> [(String, FilePath)]            -- ^ (parser name, config path)
                -> Currencies                      -- ^ Set of declared currencies
                -> ChartOfAccounts
                -> [FilePath]                      -- ^ List of files or masks (*.yaledger)
                -> IO [Ext Record]
parseInputFiles options parsers configs currs coa masks = do
    $debugIO $ "Start parsing input files by masks: " ++ show masks
    inputFiles <- concat <$> mapM glob masks
    $debugIO $ "Input files:\n" ++ unlines inputFiles
    records <- parallel (map loadFile inputFiles)
    traceEventIO "All files loaded."
    return $ sort $ concat records
  where
    loadFile file = do
       case lookupMask (takeFileName file) parsers of
         Nothing -> do
                    $infoIO $ "Unknown file type: " ++ file
                    return []
         Just (parserName, parser) ->
           let configFile = fromMaybe (parserName ++ ".yaml") $
                                lookup parserName configs
           in  do
               rec <- parser options configFile currs coa file
               $infoIO $ "Read " ++ show (length rec) ++ " records from " ++ file
               traceEventIO $ "File loaded: " ++ file
               return rec

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
  b <- doesFileExist path
  if b
    then do
         content <- readFile path
         case runParser Map.pAccountMap (Map.PState coa) path content of
           Right res -> return res
           Left err -> fail $ show err
    else return []

