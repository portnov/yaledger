{-# LANGUAGE OverloadedStrings #-}
module YaLedger.Parser.Currencies where

import Control.Applicative
import Data.Either
import Data.List
import Data.Dates
import qualified Data.Map as M
import Data.Yaml
import Text.Printf

import YaLedger.Types
import YaLedger.Kernel.Common
import YaLedger.Parser.Common
import YaLedger.Parser.Tables

instance FromJSON Currency where
  parseJSON (Object v) =
    Currency
      <$> v .: "symbol"
      <*> v .:? "number-code"
      <*> v .:? "alfa-code"
      <*> v .:? "precision" .!= 2
  parseJSON _ = fail "Currency: invalid object"

loadCurrencies :: FilePath -> IO [Currency]
loadCurrencies path = loadParserConfig path

