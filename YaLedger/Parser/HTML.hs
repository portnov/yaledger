{-# LANGUAGE OverloadedStrings #-}
module YaLedger.Parser.HTML where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as L
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.Yaml

import YaLedger.Types
import YaLedger.Parser.Common (loadParserConfig, readUrlLBS)
import YaLedger.Parser.Tables

data ParserConfig = ParserConfig {
    pcEncoding      :: Maybe String,
    pcTableSelector :: String,
    pcGeneric       :: GenericParserConfig
    }
  deriving (Eq, Show)

instance FromJSON ParserConfig where
  parseJSON (Object v) =
    ParserConfig
      <$> v .:? "encoding"
      <*> v .:? "selector" .!= "table"
      <*> parseGenericConfig ["encoding", "selector"] v
  parseJSON x = fail $ "HTML parser config: invalid object: " ++ show x

readHTML :: ParserConfig -> FilePath -> IO [[String]]
readHTML pc path = do
  bstr <- readUrlLBS path
  string <- convertToUtf8 path (pcEncoding pc) bstr
  let doc = readString [withParseHTML yes, withWarnings no] string
  table <- runX $ getTable (pcTableSelector pc) doc
  return $ map (map clean) table

clean :: String -> String
clean str = filter (`notElem` "\r\n") str

-- | Get needed <table> tag from HTML document.
-- Here all magic goes :)
getTable :: String -> IOSArrow a (NTree XNode) -> IOSArrow a [String]
getTable selector doc =
  doc >>> css selector >>> css "tr" >>> listA (css "td" >>> deep getText >. concat)

parseHTML :: LedgerOptions
          -> ParserConfig
          -> FilePath
          -> Currencies
          -> ChartOfAccounts
          -> [[String]]
          -> IO [Ext Record]
parseHTML opts pc path currs coa table =
  zipWithM (convertRow opts (pcGeneric pc) currs coa path) [1..] table

loadHTML :: LedgerOptions -> FilePath -> Currencies -> ChartOfAccounts -> FilePath -> IO [Ext Record]
loadHTML opts configPath currs coa htmlPath = do
  config <- loadParserConfig configPath 
  table <- readHTML config htmlPath
  let goodRows = filterRows (pcRowsFilter $ pcGeneric config) table
  parseHTML opts config htmlPath currs coa goodRows

