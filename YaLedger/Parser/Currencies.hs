{-# LANGUAGE OverloadedStrings #-}
module YaLedger.Parser.Currencies where

import Control.Applicative
import Data.Yaml

import YaLedger.Types
import YaLedger.Parser.Common

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

