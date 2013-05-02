{-# LANGUAGE OverloadedStrings #-}
module YaLedger.Parser.CSV where

import Control.Applicative
import Control.Monad
import Data.Yaml
import qualified Data.ByteString.Lazy as L

import YaLedger.Types
import YaLedger.Parser.Common (loadParserConfig)
import YaLedger.Parser.Tables

data ParserConfig = ParserConfig {
    pcEncoding      :: Maybe String,
    pcSeparator  :: Char,
    pcGeneric    :: GenericParserConfig
    }
  deriving (Eq, Show)

instance FromJSON ParserConfig where
  parseJSON (Object v) =
    ParserConfig
      <$> v .:? "encoding"
      <*> v .:? "separator" .!= ','
      <*> parseGenericConfig [] v
  parseJSON x = fail $ "CSV parser config: invalid object: " ++ show x

csvCells :: Char -> String -> [[String]]
csvCells sep str = map parseRow $ lines str
  where
    parseRow = split sep

parseCSV :: LedgerOptions
         -> ParserConfig
         -> FilePath
         -> Currencies
         -> ChartOfAccounts
         -> String
         -> IO [Ext Record]
parseCSV opts pc path currs coa str =
  let rows = csvCells (pcSeparator pc) str
      goodRows = filterRows (pcRowsFilter $ pcGeneric pc) rows
  in  zipWithM (convertRow opts (pcGeneric pc) currs coa path) [1..] goodRows

loadCSV :: LedgerOptions -> FilePath -> Currencies -> ChartOfAccounts -> FilePath -> IO [Ext Record]
loadCSV opts configPath currs coa csvPath = do
  config <- loadParserConfig configPath 
  bstr <- L.readFile csvPath
  csv <- convertToUtf8 csvPath (pcEncoding config) bstr
  parseCSV opts config csvPath currs coa csv

