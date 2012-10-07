{-# LANGUAGE OverloadedStrings #-}
module YaLedger.Parser.Tables where

import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Decimal
import Data.String
import Data.Yaml
import Data.Dates.Formats hiding (Fixed)
import Text.Regex.PCRE
import System.FilePath
import System.Environment.XDG.BaseDir

import YaLedger.Types

data RowsFilter =
  RowsFilter {
    rfField :: Int,
    rfRegexp :: String }
  deriving (Eq, Show)

data FieldConfig =
    FieldConfig {
      fcField  :: Int,
      fcRegexp :: Maybe String,
      fcOptional :: Bool,
      fcRules  :: [(String, String)] }
  | FixedValue String
  deriving (Eq, Show)

defaultFieldConfig :: Int -> FieldConfig
defaultFieldConfig n =
  FieldConfig {
    fcField = n,
    fcRegexp = Nothing,
    fcOptional = False,
    fcRules = [] }

data GenericParserConfig = GenericParserConfig {
    pcDate       :: FieldConfig,
    pcDateFormat :: String,
    pcCurrency   :: FieldConfig,
    pcAmount     :: FieldConfig,
    pcAccount    :: FieldConfig,
    pcAccount2   :: Maybe FieldConfig,
    pcRowsFilter :: [RowsFilter],
    pcOther      :: [(String, FieldConfig)] }
  deriving (Eq, Show)

instance FromJSON RowsFilter where
  parseJSON (Object v) =
    RowsFilter
      <$> v .:? "field" .!= 1
      <*> v .: "regexp"
  parseJSON _ = fail "Rows filter: invalid object"

instance FromJSON GenericParserConfig where
  parseJSON (Object v) = parseGenericConfig [] v
  parseJSON _ = fail "Generic parser config: invalid object"

parseGenericConfig :: [T.Text] -> Object -> Parser GenericParserConfig
parseGenericConfig reserved v =
  GenericParserConfig
      <$> v .:  "date"
      <*> v .:? "dateformat" .!= "YYYY/MM/DD"
      <*> v .:? "currency"  .!= FixedValue "$"
      <*> v .:  "amount"
      <*> v .:  "account"
      <*> v .:? "account2"
      <*> v .:?  "filter" .!= []
      <*> getOther reserved v

instance FromJSON FieldConfig where
  parseJSON (Object v) =
    FieldConfig
      <$> v .: "field"
      <*> v .:? "regexp"
      <*> v .:? "optional" .!= False
      <*> getRules v
  parseJSON (String str) =
    pure $ FixedValue (T.unpack str)
  parseJSON _ = fail "Field config: invalid object"

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
   "dateformat", "date",
   "filter"]

getOther :: [T.Text] -> Object -> Parser [(String, FieldConfig)]
getOther reserved obj = do
  let pairs = H.toList obj
      pairs' = filter (\(name,_) -> name `notElem` (reservedFields ++ reserved)) pairs
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

readSum :: String -> Either String Decimal
readSum str =
  let str' = filter (`notElem` " \r\n\t") str
      list = reads str'
  in case [x | (x, "") <- list] of
       [x] -> Right x
       _   -> Left $ "Cannot parse amount: " ++ str'

filterRows :: [RowsFilter] -> [[String]] -> [[String]]
filterRows filters rows = filter (\row -> all (ok row) filters) rows
  where
    ok :: [String] -> RowsFilter -> Bool
    ok list rf =
      if length list < rfField rf
        then False
        else let str = list !! (rfField rf - 1)
             in  str =~ rfRegexp rf

-- | Convert one row of table
convertRow :: GenericParserConfig
           -> Currencies
           -> ChartOfAccounts
           -> FilePath
           -> Int
           -> [String]
           -> IO (Ext Record)
convertRow pc currs coa path rowN row = do
  let dateStr = field (pcDate pc) row
      currencyS = field (pcCurrency pc) row
      amountStr = field (pcAmount pc) row
      accountName = field (pcAccount pc) row
      account2 = case pcAccount2 pc of
                   Just fc -> Just $ field fc row
                   Nothing -> Nothing
      as = [(name, (if fcOptional fc then Optional else Exactly) $ field fc row)
            | (name, fc) <- pcOther pc,
              name `notElem` reservedFields]
      attrs = M.fromList [p | p@(n,v) <- as, v `notElem` [Exactly "", Optional ""]]

      lookupAccount path =
         case lookupTree (mkPath path) coa of
           [] -> fail $ "No such account: " ++ path
           [a] -> return a
           as -> fail "Ambigous account specification"

  currency <- case M.lookup currencyS currs of
                Nothing -> fail $ "Unknown currency: " ++ currencyS
                Just c  -> return c

  date <- case parseDateFormat (pcDateFormat pc) dateStr of
            Right d -> return d
            Left err -> fail $ dateStr ++ ":\n" ++ show err

  (s, amount) <- case head amountStr of
                   '+' -> case readSum (tail amountStr) of
                            Right x -> return (ECredit, x)
                            Left err -> fail err
                   '-' -> case readSum (tail amountStr) of
                            Right x -> return (EDebit, x)
                            Left err -> fail err
                   _   -> case readSum amountStr of
                            Right x -> return (ECredit, x)
                            Left err -> fail err

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
    WDebit  _ a -> fail $ "Invalid account type: debit instead of credit: " ++ getName a ++ " " ++ show x

dposting :: AnyAccount -> Amount -> IO [Posting Amount Debit]
dposting acc x =
  case acc of
    WDebit   _ a -> return [DPosting (Right a) x]
    WFree    _ a -> return [DPosting (Left  a) x]
    WCredit  _ a -> fail $ "Invalid account type: credit instead of debit: " ++ getName a ++ " " ++ show x

loadParserConfig :: FromJSON config => FilePath -> IO config
loadParserConfig path = do
  fullPath <- case head path of
                '/' -> return path
                _ -> do
                     configDir <- getUserConfigDir "yaledger"
                     return (configDir </> path)
  str <- B.readFile fullPath
  case decodeEither str of
    Left err -> fail $ "Cannot parse config file " ++ fullPath ++ ": " ++ err
    Right pc -> return pc

