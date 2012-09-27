{-# LANGUAGE OverloadedStrings #-}
module YaLedger.Parser.CSV where

import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.String
import Data.Yaml
import Data.String.Utils
import Data.Dates.Formats hiding (Fixed)
import Text.Regex.PCRE
import System.FilePath
import System.Environment.XDG.BaseDir

import YaLedger.Types
import YaLedger.Tree

data FieldConfig =
    FieldConfig {
      fcField :: Int,
      fcRegexp :: Maybe String,
      fcRules :: [(String, String)] }
  | FixedValue String
  deriving (Eq, Show)

defaultFieldConfig :: Int -> FieldConfig
defaultFieldConfig n =
  FieldConfig {
    fcField = n,
    fcRegexp = Nothing,
    fcRules = [] }

data ParserConfig = ParserConfig {
    pcSeparator :: String,
    pcDateFormat :: String,
    pcDate :: FieldConfig,
    pcCurrency :: FieldConfig,
    pcAmount :: FieldConfig,
    pcAccount :: FieldConfig,
    pcAccount2 :: Maybe FieldConfig,
    pcOther :: [(String, FieldConfig)]
    }
  deriving (Eq, Show)

instance FromJSON FieldConfig where
  parseJSON (Object v) =
    FieldConfig
      <$> v .: "field"
      <*> v .:? "regexp"
      <*> getRules v
  parseJSON (String str) =
    pure $ FixedValue (T.unpack str)
  parseJSON _ = fail "Invalid object"

getRules :: Object -> Parser [(String, String)]
getRules obj = do
  rec <- obj .:? "rules"
  case rec of
    Nothing -> return []
    Just (Object rules) ->
        forM (H.toList rules) $ \(name, value) ->
          case value of
            String str -> return (T.unpack name, T.unpack str)
            _ -> mzero

instance FromJSON ParserConfig where
  parseJSON (Object v) =
    ParserConfig
      <$> v .:? "separator" .!= ","
      <*> v .:? "dateformat" .!= "YYYY/MM/DD"
      <*> v .:  "date"
      <*> v .:? "currency"  .!= FixedValue "$"
      <*> v .:  "amount"
      <*> v .:  "account"
      <*> v .:? "account2"
      <*> getOther v

reservedFields :: IsString s => [s]
reservedFields =
  ["separator", "currency", "amount",
   "account", "account2",
   "dateformat", "date"]

getOther :: Object -> Parser [(String, FieldConfig)]
getOther obj = do
  let pairs = H.toList obj
      pairs' = filter (\(name,_) -> name `notElem` reservedFields) pairs
  forM pairs' $ \(name, value) -> do
      fc <- parseJSON value
      return (T.unpack name, fc)

loadParserConfig :: FilePath -> IO ParserConfig
loadParserConfig path = do
  fullPath <- case head path of
                '/' -> return path
                _ -> do
                     configDir <- getUserConfigDir "yaledger"
                     return (configDir </> path)
  str <- B.readFile fullPath
  case decode str of
    Nothing -> fail $ "Cannot parse config file " ++ fullPath
    Just pc -> return pc

csv :: String -> String -> [[String]]
csv sep str = map parseRow $ lines str
  where
    parseRow = split sep

parseCSV :: ParserConfig -> FilePath -> ChartOfAccounts -> String -> IO [Ext Record]
parseCSV pc path coa str = zipWithM (convert pc coa path) [1..] $ csv (pcSeparator pc) str

field :: FieldConfig -> [String] -> String
field (FixedValue str) _ = str
field fc row =
  let str = row !! (fcField fc - 1)
      part = case fcRegexp fc of
               Nothing -> str
               Just regexp -> str =~ regexp
      check (res, regexp) =
          if part =~ regexp
            then [res]
            else []
  in  case fcRules fc of
        [] -> part
        rules -> case concatMap check rules of
                   [] -> ""
                   (x:_) -> x

readSum str = read $ filter (/= ' ') str

convert :: ParserConfig -> ChartOfAccounts -> FilePath -> Int -> [String] -> IO (Ext Record)
convert pc coa path rowN row = do
  let dateStr = field (pcDate pc) row
      currency = field (pcCurrency pc) row
      amountStr = field (pcAmount pc) row
      accountName = field (pcAccount pc) row
      account2 = case pcAccount2 pc of
                   Just fc -> Just $ field fc row
                   Nothing -> Nothing
      as = [(name, Exactly $ field fc row) | (name, fc) <- pcOther pc,
                                    name `notElem` reservedFields]
      attrs = M.fromList [p | p@(n,v) <- as, v /= Exactly ""]

      lookupAccount path =
         case lookupTree (mkPath path) coa of
           [] -> fail $ "No such account: " ++ path
           [a] -> return a
           as -> fail "Ambigous account specification"

  date <- case parseDateFormat (pcDateFormat pc) dateStr of
            Right d -> return d
            Left err -> fail $ show err

  (s, amount) <- case head amountStr of
                   '+' -> return (ECredit, readSum (tail amountStr))
                   '-' -> return (EDebit,  readSum (tail amountStr))
                   _   -> return (ECredit, readSum amountStr)

  acc <- lookupAccount accountName
  acc2 <- case account2 of
            Nothing -> return Nothing
            Just path -> Just <$> lookupAccount path

  entry <- case s of
             ECredit -> do
               posting <- cposting acc (amount :# currency)
               corr <- case acc2 of
                         Just acc -> dposting acc (amount :# currency)
                         Nothing  -> return []
               return $ UEntry corr posting Nothing []
             EDebit -> do
               posting <- dposting acc (amount :# currency)
               corr <- case acc2 of
                         Just acc -> cposting acc (amount :# currency)
                         Nothing  -> return []
               return $ UEntry posting corr Nothing []
  let pos = newPos path rowN 1
  return $ Ext date pos attrs (Transaction $ TEntry entry)

cposting :: AnyAccount -> Amount -> IO [Posting Amount Credit]
cposting acc x =
  case acc of
    WCredit _ a -> return [CPosting (Right a) x]
    WFree   _ a -> return [CPosting (Left  a) x]
    WDebit  _ _ -> fail $ "Invalid account type: debit instead of credit"

dposting :: AnyAccount -> Amount -> IO [Posting Amount Debit]
dposting acc x =
  case acc of
    WDebit  _ a -> return [DPosting (Right a) x]
    WFree   _ a -> return [DPosting (Left  a) x]
    WCredit  _ _ -> fail $ "Invalid account type: credit instead of debit"

loadCSV :: FilePath -> ChartOfAccounts -> FilePath -> IO [Ext Record]
loadCSV configPath coa csvPath = do
  config <- loadParserConfig configPath 
  csv <- readFile csvPath
  parseCSV config csvPath coa csv

