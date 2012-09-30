{-# LANGUAGE OverloadedStrings #-}
module YaLedger.Parser.HTML where

import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy as L
import qualified Codec.Text.IConv as IConv
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.Yaml

import YaLedger.Types
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
      <*> parseGenericConfig v

readHTML :: ParserConfig -> FilePath -> IO [[String]]
readHTML pc path = do
  bstr <- L.readFile path
  let string = T.unpack $ E.decodeUtf8 $ case pcEncoding pc of
                                          Nothing -> bstr
                                          Just encoding -> IConv.convert encoding "UTF8" bstr
  let doc = readString [withParseHTML yes, withWarnings no] string
  runX $ getTable (pcTableSelector pc) doc

-- | Get needed <table> tag from HTML document.
-- Here all magic goes :)
getTable :: String -> IOSArrow a (NTree XNode) -> IOSArrow a [String]
getTable selector doc =
  doc >>> css selector >>> css "tr" >>> listA (css "td" >>> deep getText >. concat)

parseHTML :: ParserConfig
          -> FilePath
          -> ChartOfAccounts
          -> [[String]]
          -> IO [Ext Record]
parseHTML pc path coa table =
  zipWithM (convertRow (pcGeneric pc) coa path) [1..] table

loadHTML :: FilePath -> ChartOfAccounts -> FilePath -> IO [Ext Record]
loadHTML configPath coa htmlPath = do
  config <- loadParserConfig configPath 
  table <- readHTML config htmlPath
  parseHTML config htmlPath coa table

