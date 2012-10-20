{-# LANGUAGE OverloadedStrings #-}
module YaLedger.Config where

import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Yaml
import Data.Dates
import System.FilePath
import System.Directory
import System.Environment.XDG.BaseDir
import System.Environment.XDG.UserDir
import Text.Parsec hiding ((<|>))
import System.IO.Unsafe (unsafePerformIO)

import YaLedger.Types.Transactions
import YaLedger.Types.Config
import YaLedger.Attributes
import YaLedger.Logger
import YaLedger.Parser.Common (pAttributeValue)
import YaLedger.Processor.Duplicates

instance Monoid LedgerOptions where
  mempty = Help
  mappend Help o = o
  mappend o Help = o
  mappend o1 o2 =
    LedgerOptions {
      mainConfigPath = mainConfigPath o2 `mplus` mainConfigPath o1,
      chartOfAccounts = chartOfAccounts o2 `mplus` chartOfAccounts o1,
      accountMap  = accountMap  o2 `mplus` accountMap o1,
      currenciesList = currenciesList o2 `mplus` currenciesList o1,
      files = if null (files o2) then files o1 else files o2,
      query = query o1 `mappend` query o2,
      reportsQuery = reportsQuery o1 `mappend` reportsQuery o2,
      reportsInterval = reportsInterval o2 `mplus` reportsInterval o1,
      logSeverity = min (logSeverity o1) (logSeverity o2),
      parserConfigs = parserConfigs o1 ++ parserConfigs o2,
      deduplicationRules = if null (deduplicationRules o2)
                             then deduplicationRules o1
                             else deduplicationRules o2,
      reportParams = if null (reportParams o2) then reportParams o1 else reportParams o2 }

instance Monoid Query where
  mempty = Query Nothing Nothing False M.empty
  mappend q1 q2 =
    Query {
      qStart = qStart q1 `mappend` qStart q2,
      qEnd   = qEnd   q1 `mappend` qEnd   q2,
      qAllAdmin = qAllAdmin q1 || qAllAdmin q2,
      qAttributes = qAttributes q1 `M.union` qAttributes q2 }

instance FromJSON LedgerOptions where
  parseJSON (Object v) =
    LedgerOptions
      <$> return Nothing
      <*> v .:? "chart-of-accounts"
      <*> v .:? "accounts-map"
      <*> v .:? "currencies"
      <*> v .:?  "files" .!= []
      <*> v .:? "query" .!= mempty
      <*> v .:? "reports-interval"
      <*> v .:? "reports" .!= mempty
      <*> v .:? "debug" .!= WARNING
      <*> (parseConfigs =<< (v .:? "parsers"))
      <*> v .:? "deduplicate" .!= []
      <*> return []
  parseJSON _ = fail "LedgerOptions: invalid object"

instance FromJSON DateInterval where
  parseJSON (String text) =
    case runParser pDateInterval () (T.unpack text) (T.unpack text) of
      Left err -> fail $ show err
      Right interval -> return interval
  parseJSON _ = fail "Date interval: invalid object"

instance FromJSON DeduplicationRule where
  parseJSON (Object v) =
    DeduplicationRule
      <$> v .:? "condition" .!= M.empty
      <*> v .: "check-attributes"
      <*> v .: "action"
  parseJSON _ = fail "Deduplication rule: invalid object"

instance FromJSON CheckAttribute where
  parseJSON (String text) =
    case text of
      "credit-account" -> return CCreditAccount
      "debit-account"  -> return CDebitAccount
      "date"           -> return (CDate 0)
      "amount"         -> return (CAmount 0)
      _ -> pure (CAttribute $ T.unpack text)
  parseJSON (Object v) =
        (CDate <$> v .: "date")
    <|> (CAmount <$> v .: "amount")
  parseJSON _ = fail "Check attributes: invalid object"

instance FromJSON DAction where
  parseJSON (String text) =
    case text of
      "error"     -> return DError
      "warning"   -> return DWarning
      "duplicate" -> return DDuplicate
      "ignore-new" -> return DIgnoreNew
      "delete-old" -> return DDeleteOld
      _ -> fail $ "Unknown deduplication action: " ++ T.unpack text
  parseJSON (Object v) =
    pSetAttributes =<< parsePairs =<< v .: "set-attributes"
  parseJSON _ = mzero

pSetAttributes :: [(String, String)] -> Parser DAction
pSetAttributes pairs = do
  let pset "" = SFixed ""
      pset ('$':name) = SExactly name
      pset ('?':name) = SOptional name
      pset x = SFixed x
      
      sets = [name := pset value | (name, value) <- pairs]

  return $ DSetAttributes sets

instance FromJSON Priority where
  parseJSON (String text) =
    case text of
      "debug"     -> return DEBUG
      "info"      -> return INFO
      "notice"    -> return NOTICE
      "warning"   -> return WARNING
      "error"     -> return ERROR
      "critical"  -> return CRITICAL
      "alert"     -> return ALERT
      "emergency" -> return EMERGENCY
      _ -> fail $ "Unknown debug level: " ++ T.unpack text

instance FromJSON Query where
  parseJSON (Object v) =
    Query
      <$> v .:? "start"
      <*> v .:? "end"
      <*> v .:? "all-admin" .!= False
      <*> parseAttrs v
  parseJSON _ = fail "Invalid object"

instance FromJSON DateTime where
  parseJSON (String text) =
    let now = unsafePerformIO getCurrentDateTime
    in case parseDate now (T.unpack text) of
         Left err -> fail $ show err
         Right date -> return date

parseAttrs :: Object -> Parser Attributes
parseAttrs obj = do
  let pairs = H.toList obj
      pairs' = filter (\(name,_) -> name `notElem` ["start","end"]) pairs

  attrs <- forM pairs' $ \(name,value) -> do
               value' <- parseValue value
               return (T.unpack name, value')
  return $ M.fromList attrs

instance FromJSON AttributeValue where
  parseJSON v = parseValue v

parseValue :: Value -> Parser AttributeValue
parseValue (String text) = do
  let str = T.unpack text
  case runParser pAttributeValue () str str of
    Left err -> fail $ show err
    Right val -> return val
parseValue _ = fail "Invalid object type in attribute value"

parsePairs :: Object -> Parser [(String, String)]
parsePairs obj = do
  let pairs = H.toList obj
  return [(T.unpack name, T.unpack value) | (name, String value) <- pairs]

parseConfigs :: Maybe Object -> Parser [(String, FilePath)]
parseConfigs Nothing = return []
parseConfigs (Just obj) = parsePairs obj

getDefaultLedgerOptions :: IO LedgerOptions
getDefaultLedgerOptions = do
  now <-  getCurrentDateTime
  configDir <- getUserConfigDir "yaledger"
  documents <- getUserDir "DOCUMENTS"
  let inputFile = documents </> "yaledger" </> "default.yaledger"
  let emptyQuery =  Query {
                      qStart = Nothing,
                      qEnd   = Just now,
                      qAllAdmin   = False,
                      qAttributes = M.empty }
  return $ LedgerOptions {
        mainConfigPath = Just (configDir </> "yaledger.yaml"),
        chartOfAccounts = Just (configDir </> "default.accounts"),
        accountMap  = Just (configDir </> "default.map"),
        currenciesList = Just (configDir </> "currencies.yaml"),
        files = [inputFile],
        query = emptyQuery,
        reportsInterval = Nothing,
        reportsQuery = emptyQuery,
        logSeverity = WARNING,
        parserConfigs = [],
        deduplicationRules = [],
        reportParams = ["balance"] }

loadConfig :: FilePath -> IO LedgerOptions
loadConfig configFile = do
  zero <- getDefaultLedgerOptions
  exist <- doesFileExist configFile
  if not exist
    then return zero
    else do
        str <- B.readFile configFile
        case decodeEither str of
          Left err -> fail $ "Cannot parse config file: " ++ configFile ++ ": " ++ err
          Right options -> return (zero `mappend` options)

