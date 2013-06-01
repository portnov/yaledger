{-# LANGUAGE TemplateHaskell #-}
-- | This module contains high-level functions for parsing input files
module YaLedger.Parser
  (parseInputFiles,
   readCoA,
   readAMap
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.ParallelIO
import Data.Maybe
import Data.List
import qualified Data.Text.IO as TIO
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

-- | Lookup for parser by file mask
lookupMask :: FilePath -> [ParserSpec] -> Maybe ParserSpec
lookupMask _ [] = Nothing
lookupMask file (spec:xs)
  | match (compile (psMask spec)) file = Just spec
  | otherwise                          = lookupMask file xs

-- | Parse all input files using corresponding parsers
parseInputFiles :: LedgerOptions
                -> Currencies                      -- ^ Set of declared currencies
                -> ChartOfAccounts
                -> [FilePath]                      -- ^ List of files or masks (*.yaledger)
                -> IO [Ext Record]
parseInputFiles options currs coa masks = do
    $debugIO $ "Start parsing input files by masks: " ++ show masks
    inputFiles <- concat <$> mapM glob masks
    $debugIO $ "Input files:\n" ++ unlines inputFiles
    records <- parallel (map loadFile inputFiles)
    traceEventIO "All files loaded."
    return $ sort $ concat records
  where
    parsers = parserConfigs options
    loadFile file = do
       case lookupMask (takeFileName file) parsers of
         Nothing -> do
                    $infoIO $ "Unknown file type: " ++ file
                    return []
         Just spec ->
           let configFile = psConfigPath spec
               parse      = psParser spec
           in  do
               rec <- parse options configFile currs coa file
               $infoIO $ "Read " ++ show (length rec) ++ " records from " ++ file ++ " (parser config: " ++ configFile ++ ")"
               traceEventIO $ "File loaded: " ++ file
               return rec

-- | Read chart of accounts from file
readCoA :: Currencies -> FilePath -> IO ChartOfAccounts
readCoA currs path = do
  content <- TIO.readFile path
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
         content <- TIO.readFile path
         case runParser Map.pAccountMap (Map.PState coa) path content of
           Right res -> return res
           Left err -> fail $ show err
    else return []

