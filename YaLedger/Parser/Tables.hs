{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
module YaLedger.Parser.Tables where

import Control.Applicative
import Control.Monad
import qualified Control.Exception as EX
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy as L
import qualified Codec.Text.IConv as IConv
import Data.Maybe
import Data.Decimal
import Data.String
import Data.Yaml
import Data.Dates.Formats hiding (Fixed)
import Text.Regex.PCRE
import Foreign.C.Error (Errno (..))

import YaLedger.Types
import YaLedger.Logger

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
    pcThousandsSep :: Char,
    pcDecimalSep   :: Char,
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
      <*> v .:? "thousands-separator" .!= ' '
      <*> v .:? "decimal-separator"   .!= '.'
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
            Bool b     -> return (T.unpack name, show b)
            x -> fail $ "Tables.getRules: invalid object: " ++ show x

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
        rules -> let rules' = filter (\p -> fst p /= "default") rules
                     dflt = fromMaybe "" $ lookup "default" rules
                 in case concatMap check rules of
                      [] -> dflt
                      (x:_) -> x

readSum :: Char -> Char -> String -> Either String Decimal
readSum thousandsSep decimalSep str =
  let str' = filter (`notElem` (thousandsSep: " \r\n\t")) str
      dot c
        | decimalSep == c = '.'
        | otherwise       = c
      readD s = case [x | (x, "") <- reads s] of
                  [x] -> Right x
                  _   -> Left $ "Cannot parse amount: " ++ s

  in case length $ filter (== decimalSep) str' of
       0 -> Decimal 0 <$> readD str'
       1 -> readD $ map dot str'
       _ -> Left $ "More than one decimal separator in number: " ++ str
  
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
      attribute fc value =
        case fc of
          FixedValue ('?':str) -> Optional str
          FixedValue str       -> Exactly str
          _ -> if fcOptional fc
                 then Optional value
                 else Exactly  value
      as = [(name, attribute fc $ field fc row)
            | (name, fc) <- pcOther pc,
              name `notElem` reservedFields]
      attrs = M.fromList [p | p@(n,v) <- as, v `notElem` [Exactly "", Optional ""]]

      lookupAccount path =
         case lookupTree (mkPath path) coa of
           [] -> fail $ "No such account: " ++ path ++ "\n" ++ unwords row
           [a] -> return a
           as -> fail "Ambigous account specification"

  currency <- case M.lookup currencyS currs of
                Nothing -> fail $ "Unknown currency: " ++ currencyS
                Just c  -> return c

  date <- case parseDateFormat (pcDateFormat pc) dateStr of
            Right d -> return d
            Left err -> fail $ dateStr ++ ":\n" ++ show err

  (s, amount) <- case head amountStr of
                   '+' -> case readSum (pcThousandsSep pc) (pcDecimalSep pc) (tail amountStr) of
                            Right x -> return (ECredit, x)
                            Left err -> fail err
                   '-' -> case readSum (pcThousandsSep pc) (pcDecimalSep pc) (tail amountStr) of
                            Right x -> return (EDebit, x)
                            Left err -> fail err
                   _   -> case readSum (pcThousandsSep pc) (pcDecimalSep pc) amountStr of
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
  return $ Ext date 0 pos attrs (Transaction $ TEntry entry)

cposting :: AnyAccount -> Amount -> IO [Posting Amount Credit]
cposting acc x =
  case acc of
    WCredit _ a -> return [CPosting (Right a) x False]
    WFree   _ a -> return [CPosting (Left  a) x False]
    WDebit  _ a -> fail $ "Invalid account type: debit instead of credit: " ++ getName a ++ " " ++ show x

dposting :: AnyAccount -> Amount -> IO [Posting Amount Debit]
dposting acc x =
  case acc of
    WDebit   _ a -> return [DPosting (Right a) x False]
    WFree    _ a -> return [DPosting (Left  a) x False]
    WCredit  _ a -> fail $ "Invalid account type: credit instead of debit: " ++ getName a ++ " " ++ show x

instance Show Errno where
  show (Errno x) = show x

deriving instance Show IConv.ConversionError

convertToUtf8 :: FilePath -> Maybe String -> L.ByteString -> IO String
convertToUtf8 path Nothing str = return $ TL.unpack $ E.decodeUtf8 str
convertToUtf8 path (Just encoding) str =
  (TL.unpack . E.decodeUtf8) <$> (case IConv.convertStrictly encoding "UTF8" str of
                                    Right err -> fail $ "Error converting " ++ path ++ " from " ++ encoding ++ " to UTF8: " ++ show err
                                    Left res -> return res )
                                  

