{-# LANGUAGE OverloadedStrings #-}
module YaLedger.Parser.CSV where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import Data.String.Utils
import Data.Yaml
import System.FilePath
import System.Environment.XDG.BaseDir

import YaLedger.Types
import YaLedger.Parser.Tables

data ParserConfig = ParserConfig {
    pcSeparator  :: String,
    pcGeneric    :: GenericParserConfig
    }
  deriving (Eq, Show)

instance FromJSON ParserConfig where
  parseJSON (Object v) =
    ParserConfig
      <$> v .:? "separator" .!= ","
      <*> parseGenericConfig [] v

csvCells :: String -> String -> [[String]]
csvCells sep str = map parseRow $ lines str
  where
    parseRow = split sep

parseCSV :: ParserConfig
         -> FilePath
         -> ChartOfAccounts
         -> String
         -> IO [Ext Record]
parseCSV pc path coa str =
  let rows = csvCells (pcSeparator pc) str
      goodRows = filterRows (pcRowsFilter $ pcGeneric pc) rows
  in  zipWithM (convertRow (pcGeneric pc) coa path) [1..] goodRows

loadCSV :: FilePath -> ChartOfAccounts -> FilePath -> IO [Ext Record]
loadCSV configPath coa csvPath = do
  config <- loadParserConfig configPath 
  csv <- readFile csvPath
  parseCSV config csvPath coa csv

