{-# LANGUAGE RecordWildCards, OverloadedStrings, BangPatterns, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell #-}
-- | Parser for `native' transactions journal file
module YaLedger.Parser.Transactions
  (loadTransactions,
   Parser,
   ext, pSetRate, emptyPState
  ) where

import Control.Applicative hiding (many, (<|>), optional)
import Data.Maybe
import Data.Either
import Data.List
import Data.Dates
import Data.Dates.Formats (formatParser, pFormat)
import qualified Data.Map as M
import Data.Yaml hiding (Parser)
import Text.Parsec hiding (space, spaces)
import Text.Printf
import System.Directory

import YaLedger.Types
import YaLedger.Processor.Templates
import YaLedger.Kernel.Common
import YaLedger.Parser.Common
import YaLedger.Logger

data PState = PState {
  getCoA :: ChartOfAccounts,
  currentDate :: DateTime,
  getThousandsSeparator :: Char,
  getDecimalSeparator :: Char,
  dateTimeParser :: Maybe (Parser DateTime),
  declaredCurrencies :: Currencies,
  defaultAttrs :: Attributes,
  rootPath :: Path,
  getOptions :: LedgerOptions,
  templates :: M.Map String Int -- ^ Number of parameters
  }

type Parser a = Parsec String PState a

data NativeParserConfig = NativeParserConfig {
    thousandsSeparator :: Char,
    decimalSeparator :: Char,
    nativeDateFormat :: Maybe String }
  deriving (Eq, Show)

defaultNativeParserConfig :: NativeParserConfig
defaultNativeParserConfig =
  NativeParserConfig {
    thousandsSeparator = ' ',
    decimalSeparator   = '.',
    nativeDateFormat   = Nothing }

instance FromJSON NativeParserConfig where
  parseJSON (Object v) =
    NativeParserConfig
      <$> v .:? "thousands-separator" .!= ' '
      <*> v .:? "decimal-separator" .!= '.'
      <*> v .:? "dateformat"
  parseJSON Null = return defaultNativeParserConfig
  parseJSON x = fail $ "NativeParserConfig: invalid object: " ++ show x

space = oneOf " \t"
spaces = many space

emptyPState :: DateTime -> LedgerOptions -> ChartOfAccounts -> Currencies -> Maybe String -> PState
emptyPState now opts coa currs mbFormat =
    PState {
      getCoA = coa,
      currentDate = now,
      getThousandsSeparator = ' ',
      getDecimalSeparator = '.',
      dateTimeParser = dParser,
      declaredCurrencies = currs,
      defaultAttrs = M.empty,
      rootPath = [],
      getOptions = opts,
      templates = M.empty }
  where
    dParser = case mbFormat of
                 Nothing -> Nothing
                 Just str -> case runParser pFormat True str str of
                               Left err     -> error $ "Cannot use specified date format: " ++ show err
                               Right format -> Just $! formatParser format

{-# NOINLINE emptyPState #-}

pPathRelative :: Parser Path
pPathRelative = do
  st <- getState
  path <- pPath
  return $ rootPath st ++ path

addTemplate :: String -> Transaction Param -> Parser ()
addTemplate name tran = do
    st <- getState
    putState $ st {templates = M.insert name (nParams tran) (templates st)}

getNParams :: String -> Parser Int
getNParams name = do
  st <- getState
  case M.lookup name (templates st) of
    Nothing -> fail $ "Template was not defined (yet?): " ++ name
    Just n  -> return n

recordDate :: Parser DateTime
recordDate = do
    st <- getState
    try (datetime $ dateTimeParser st) <|> timeOnly
  where
    datetime mbP = do
       st <- getState
       let now = currentDate st
       dt <- case mbP of
               Nothing     -> pDate now
               Just parser -> try parser <|> pDate now
       putState $ st {currentDate = dt}
       mbT <- optionMaybe $ do
                comma
                spaces
                pTime
       case mbT of
         Nothing -> return dt
         Just t  -> return (dt `addTime` t)

    timeOnly = do
       st <- getState
       let now = currentDate st
       t <- pTime
       return $ date now `addTime` t

    date d = d {hour = 0, minute = 0, second = 0}

pDateTime' :: DateTime -> Parser DateTime
pDateTime' now = do
  date <- pDate now
  x <- optionMaybe $ do
         comma
         spaces
         pTime
  case x of
    Nothing -> return date
    Just t  -> return (date `addTime` t)

pSpecial :: Parser ()
pSpecial = try setAttributes <|> setRoot
  where
    setAttributes = do
      reserved "attributes"
      spaces
      attrs <- braces pAttributes
      st <- getState
      putState $ st {defaultAttrs = attrs}

    setRoot = do
      reserved "root"
      spaces
      path <- pPath
      x <- getCoAItem getPosition (getCoA <$> getState) path
      case x of
        Branch {} -> do
                     st <- getState
                     putState $ st {rootPath = path}
        _ -> fail $ "This is an account, not accounts group:" ++ intercalate "/" path

pRecords :: Parser [Ext Record]
pRecords = do
  rs <- (try (pSpecial >> return Nothing) <|> (Just <$> pRecord))  `sepEndBy1` (many newline <?> "N2")
  eof
  return (catMaybes rs)

insertAttr :: (String -> AttributeValue) -> String -> Maybe String -> Attributes -> Attributes
insertAttr _ _ Nothing attrs = attrs
insertAttr constr name (Just value) attrs = M.insert name (constr value) attrs

ext :: Parser a -> Parser (Ext a)
ext p = do
  pos <- getPosition
  char '@'
  space
  date <- recordDate
  spaces
  mbStatus <- optionMaybe $ do
                r <- many1 $ oneOf "!#/\\:.*?%-_+=@&<>"
                spaces
                return r
  mbCategory <- optionMaybe $ do
                  char '('
                  r <- many1 $ noneOf "\n\r)"
                  char ')'
                  return r
  spaces
  mbDescription <- optionMaybe $ many1 $ noneOf "\n\r"
  newline
  st <- getState
  let defAttrs = defaultAttrs st
  attrs <- option M.empty $ braces $ pAttributes
  let attrs' = insertAttr Optional "description" mbDescription $
               insertAttr Exactly  "status"      mbStatus      $
               insertAttr Exactly  "category"    mbCategory (defAttrs `M.union` attrs)
  content <- p
  return $ Ext date 0 pos attrs' content

pRecord :: Parser (Ext Record)
pRecord = try (ext pTemplate)
      <|> try (ext pRule)
      <|> try (ext pPeriodic)
      <|> try (ext pStop)
      <|> try (ext pSetRate)
      <|> ext (Transaction <$> pTransaction pAmount)

pTransaction :: Show v => Parser v -> Parser (Transaction v)
pTransaction p =
        try (pEntry p)
    <|> try (pReconciliate p)
    <|> try (pSetHold p)
    <|> try (pCloseHold p)
    <|> pCall

pTemplate :: Parser Record
pTemplate = do
  reserved "template"
  name <- identifier
  tran <- pTemplateTran
  addTemplate name tran
  return $ Template name tran

pRule :: Parser Record
pRule = do
  reserved "rule"
  spaces
  name <- identifier
  spaces
  reservedOp "="
  spaces
  reserved "when"
  spaces
  condition <- try pCondition
  spaces
  reserved "do"
  spaces
  tran <- pTemplateTran
  return $ RuleR name condition tran

pTemplateTran :: Parser (Transaction Param)
pTemplateTran =
      try (pEntry param)
  <|> pReconciliate param

pCondition :: Parser Condition
pCondition = do
    action <- try (reserved "credit" >> return (Just ECredit))
          <|> try (reserved "debit"  >> return (Just EDebit))
          <|> (reserved "use" >> return Nothing)
    spaces
    objects <- pRuleObject `sepBy1` comma
    spaces
    value <- try (cmp MoreThan ">")
         <|> try (cmp LessThan "<")
         <|> try (cmp Equals   "==")
         <|> return AnyValue
    attrs <- option M.empty pAttrs
    return $ Condition {
               cAccounts = lefts  objects,
               cGroups   = rights objects,
               cAction   = action,
               cAttributes = attrs,
               cValue    = value }
  where
    cmp constructor op = do
      reservedOp op
      spaces
      x <- pAmount
      return $ constructor x

    pAttrs = do
      spaces
      reserved "with"
      spaces
      braces pAttributes

pRuleObject :: Parser (Either AccountID GroupID)
pRuleObject = do
  path <- pPathRelative
  item <- getCoAItem getPosition (getCoA <$> getState) path
  case item of
    Leaf {..}   -> return $ Left  (getID leafData)
    Branch {..} -> return $ Right (getID branchData)

pPeriodic :: Parser Record
pPeriodic = do
  reserved "periodic"
  spaces
  name <- identifier
  spaces
  reservedOp "="
  spaces
  reserved "every"
  spaces
  interval <- pInterval
  spaces
  reserved "do"
  tran <- pTransaction pAmount
  return $ Periodic name interval tran

pStop :: Parser Record
pStop = do
  reserved "stop"
  name <- identifier
  return $ StopPeriodic name

pInterval :: Parser DateInterval
pInterval = try (go Days "day")
        <|> try (go Weeks "week")
        <|> try (go Months "month")
        <|> go Years "year"
  where
    go constructor str = do
      n <- natural
      spaces
      string str
      optional $ char 's'
      return (constructor n)

holdUsage :: Parser HoldUsage
holdUsage =
      try (reserved "try" >> spaces >> reserved "use" >> spaces >> return TryUseHold)
  <|> try (reserved "use" >> spaces >> return UseHold)
  <|> return DontUseHold

pEntry :: Show v => Parser v -> Parser (Transaction v)
pEntry p = do
  rs <- many1 (pPostingHold p)
  let es = rights rs
  corr <- case lefts rs of
            [] -> return Nothing
            [(use,x)] -> return $ Just (x,use)
            _ -> fail $ "Only one line in entry can be used without amount"
  let cr = [p | CP p <- es]
      dt = [p | DP p <- es]
  return $ TEntry $ UEntry dt cr corr []

pSetHold :: Show v => Parser v -> Parser (Transaction v)
pSetHold p = do
    hs <- many1 pAnyHold
    return $ THold (lefts hs) (rights hs)
  where
    pAnyHold = do
      reserved "hold"
      spaces
      anyPosting <- pPosting p
      case anyPosting of
        CP cposting -> return $ Left  (Hold cposting Nothing)
        DP dposting -> return $ Right (Hold dposting Nothing)

pCloseHold :: Show v => Parser v -> Parser (Transaction v)
pCloseHold p = foldl go zero <$> (many1 $ do
      reserved "close"
      op <- optionMaybe $ do
              spaces
              reservedOp "<="
      let searchLesser = isJust op
      spaces
      r <- pPosting p
      attrs <- option M.empty $ do
                 spaces
                 reserved "with"
                 spaces
                 braces pAttributes 
      case r of
        CP p -> return $ Left  $ CloseHold (Hold p Nothing) searchLesser attrs
        DP p -> return $ Right $ CloseHold (Hold p Nothing) searchLesser attrs )
  where
    zero = TCloseHolds [] []
    go (TCloseHolds cr dr) (Left h)  = TCloseHolds (h:cr) dr
    go (TCloseHolds cr dr) (Right h) = TCloseHolds cr (h:dr)

pCall :: Parser (Transaction v)
pCall = do
  reserved "call"
  spaces
  name <- identifier
  spaces
  args <- pAmount `sepEndBy` spaces
  n <- getNParams name
  if length args > n
    then fail $ printf "At call of `%s' template: too many arguments: %d instead of %d"
                       name (length args) n
    else return $ TCallTemplate name args

pReconciliate :: Parser v -> Parser (Transaction v)
pReconciliate p = do
    reserved "reconciliate"
    spaces
    path <- pPathRelative
    account <- getAccount getPosition (getCoA <$> getState) path 
    spaces
    btype <- option AvailableBalance $ do
               try (bt "available" AvailableBalance) <|>
                   (bt "ledger"    LedgerBalance)
    x <- p
    targetAccount <- optionMaybe $ try $ do
                       spaces
                       reserved "with"
                       spaces
                       path <- pPathRelative
                       getAccount getPosition (getCoA <$> getState) path
    msg <- optionMaybe pReconMessage
    return $ TReconciliate btype account x targetAccount msg
  where
    bt word res = do
      reserved word
      spaces
      return res

defaultReconMessage :: MessageFormat
defaultReconMessage =
  [MFixed "On reconciliation: calculated balance of ",
   MVariable "account",
   MFixed " is ",
   MVariable "calculated",
   MFixed ", but actual balance is ",
   MVariable "actual" ]

pReconMessage :: Parser ReconciliationMessage
pReconMessage = try (go "warning" RWarning) <|> (go "error" RError)
  where
    go keyword constr = do
      spaces
      reserved keyword
      msg <- option defaultReconMessage $ do
               char ':'
               spaces
               pMessageFormat
      return (constr msg)

pSetRate :: Parser Record
pSetRate = SetRate <$> many1 (do
    reserved "rate"
    spaces
    try implicit <|> explicit )
  where
    operator =
      try (reservedOp "->" >> return False) <|>
          (reservedOp "<->" >> return True)

    explicit = do
      a1 <- float
      c1 <- currency
      spaces
      reversible <- operator
      spaces
      a2 <- float
      c2 <- currency
      optional newline
      return (Explicit c1 a1 c2 a2 reversible)

    implicit = do
      c1 <- currency
      spaces
      reversible <- operator
      spaces
      c2 <- currency
      spaces
      reservedOp "="
      spaces
      reserved "via"
      spaces
      base <- currency
      optional newline
      return (Implicit c1 c2 base reversible)

pPostingHold :: Show v => Parser v -> Parser (Either (HoldUsage,AnyAccount) (AnyPosting v))
pPostingHold p = try (Right <$> posting) <|> (Left <$> account)
  where
    account = do
      spaces
      use <- holdUsage
      accPath <- pPathRelative
      acc <- getAccountT AGCredit getPosition (getCoA <$> getState) accPath
      many newline
      return (use, acc)

    posting = do
      spaces
      use <- holdUsage
      anyPosting <- pPosting p
      many newline
      case anyPosting of
        CP cposting -> return $ CP $ cposting {creditPostingUseHold = use}
        DP dposting -> return $ DP $ dposting {debitPostingUseHold  = use}

pPosting :: forall v. Show v => Parser v -> Parser (AnyPosting v)
pPosting p = try (CP <$> explicitCredit) <|> try (DP <$> explicitDebit) <|> implicit
  where
    --  Left for credit amount, Right for debit
    signed :: Parser v -> Parser (Delta v)
    signed p = do
      mbMinus <- optionMaybe $ char '-'
      val <- p
      case mbMinus of
        Nothing -> return (Increase val)
        Just _  -> return (Decrease val)

    implicit :: Parser (AnyPosting v)
    implicit = do
      spaces
      accPath <- pPathRelative
      acc <- getAccountT AGCredit getPosition (getCoA <$> getState) accPath
      spaces
      amt <- signed p
      opts <- getOptions <$> getState
      p <- autoPosting opts amt accPath acc DontUseHold
      whiteSpace
      many newline
      return p

    explicitCredit :: Parser (Posting v Credit)
    explicitCredit = do 
      spaces
      reserved "cr"
      accPath <- pPathRelative
      acc <- getAccountT AGCredit getPosition (getCoA <$> getState) accPath
      account <- case acc of
                   WFree   acc -> return $ Left acc
                   WCredit acc -> return $ Right acc
                   _ -> fail $ printf "Invalid account type: %s: debit instead of credit." (intercalate "/" accPath)
      spaces
      amount <- p
      whiteSpace
      many newline
      return $ CPosting account amount DontUseHold

    explicitDebit :: Parser (Posting v Debit)
    explicitDebit = do
      spaces
      reserved "dr"
      accPath <- pPathRelative
      acc <- getAccountT AGDebit getPosition (getCoA <$> getState) accPath
      account <- case acc of
                   WFree   acc -> return $ Left acc
                   WDebit acc -> return $ Right acc
                   _ -> fail $ printf "Invalid account type: %s: credit instead of debit." (intercalate "/" accPath)
      spaces
      amount <- p
      whiteSpace
      many newline
      return $ DPosting account amount DontUseHold

pAmount :: Parser Amount
pAmount = try numberFirst <|> currencyFirst
  where
    numberFirst = do
      n <- number getThousandsSeparator getDecimalSeparator
      c <- currency
      return $ n :# c

    currencyFirst = do
      c <- currency
      n <- number getThousandsSeparator getDecimalSeparator
      return $ n :# c

currency :: Parser Currency
currency = do
  symbol <- currencySymbol
  st <- getState
  case M.lookup symbol (declaredCurrencies st) of
    Nothing -> fail $ "Unknown currency: " ++ symbol
    Just c  -> return c

param :: Parser Param
param = try pParam <|> try pBalance <|> (Fixed <$> pAmount)
  where
    pParam = do
      char '#'
      ns <- many1 digit
      let n = read ns
      a <- optionMaybe $ try $ reservedOp "*"
      c <- case a of
             Nothing -> return 1.0
             Just _ -> float
      spaces
      d <- option (0 :# emptyCurrency) $ try $ parens $ do
               reserved "default"
               pAmount
      return $ Param n c d

    pBalance = do
      char '#'
      reserved "balance"
      a <- optionMaybe $ try $ reservedOp "*"
      c <- case a of
             Nothing -> return 1.0
             Just _  -> float
      return $ FromBalance c

-- | Read transactions from `native' format file (*.yaledger)
loadTransactions :: LedgerOptions
                 -> FilePath         -- ^ Parser config file path
                 -> Currencies
                 -> ChartOfAccounts
                 -> FilePath         -- ^ Source file path
                 -> IO [Ext Record]
loadTransactions opts configPath currs coa path = do
  ex <- doesFileExist configPath
  config <- if ex
              then do
                   $infoIO $ "Using config for native format: " ++ configPath
                   loadParserConfig configPath
              else return defaultNativeParserConfig
  content <- readFile path
  now <- getCurrentDateTime
  let !st = emptyPState now opts coa currs (nativeDateFormat config)
  case runParser pRecords st path content of
    Right res -> return res
    Left err -> fail $ show err

