{-# LANGUAGE OverloadedStrings #-}
module YaLedger.Parser.Tables where

import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.String
import Data.Yaml
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

data GenericParserConfig = GenericParserConfig {
    pcDate :: FieldConfig,
    pcDateFormat :: String,
    pcCurrency :: FieldConfig,
    pcAmount :: FieldConfig,
    pcAccount :: FieldConfig,
    pcAccount2 :: Maybe FieldConfig,
    pcOther :: [(String, FieldConfig)] }
  deriving (Eq, Show)

instance FromJSON GenericParserConfig where
  parseJSON (Object v) = parseGenericConfig v
  parseJSON _ = fail "Invalid object"

parseGenericConfig :: Object -> Parser GenericParserConfig
parseGenericConfig v =
  GenericParserConfig
      <$> v .:  "date"
      <*> v .:? "dateformat" .!= "YYYY/MM/DD"
      <*> v .:? "currency"  .!= FixedValue "$"
      <*> v .:  "amount"
      <*> v .:  "account"
      <*> v .:? "account2"
      <*> getOther v

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

convertRow :: GenericParserConfig -> ChartOfAccounts -> FilePath -> Int -> [String] -> IO (Ext Record)
convertRow pc coa path rowN row = do
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

