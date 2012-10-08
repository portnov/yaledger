{-# LANGUAGE RecordWildCards #-}
-- | Parser for `native' transactions journal file
module YaLedger.Parser.Transactions
  (loadTransactions,
   Parser,
   ext, pSetRate, emptyPState
  ) where

import Control.Applicative hiding (many, (<|>), optional)
import Data.Either
import Data.List
import Data.Dates
import qualified Data.Map as M
import Text.Parsec
import Text.Printf

import YaLedger.Types
import YaLedger.Templates
import YaLedger.Kernel.Common
import YaLedger.Parser.Common

data PState = PState {
  getCoA :: ChartOfAccounts,
  currentDate :: DateTime,
  declaredCurrencies :: Currencies,
  templates :: M.Map String Int -- ^ Number of parameters
  }

type Parser a = Parsec String PState a

emptyPState :: ChartOfAccounts -> Currencies -> IO PState
emptyPState coa currs = do
  now <- getCurrentDateTime
  return $ PState {
             getCoA = coa,
             currentDate = now,
             declaredCurrencies = currs,
             templates = M.empty }

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

pRecords :: Parser [Ext Record]
pRecords = do
  rs <- pRecord `sepEndBy1` (many newline <?> "N2")
  eof
  return rs

ext :: Parser a -> Parser (Ext a)
ext p = do
  pos <- getPosition
  char '@'
  space
  st <- getState
  date <- try $ pDateTime' (currentDate st)
  descr <- optionMaybe $ many1 $ noneOf "\n\r"
  newline
  attrs <- option M.empty $ braces $ pAttributes
  let attrs' = case descr of
                 Nothing -> attrs
                 Just s -> M.insert "description" (Optional s) attrs
  content <- p
  return $ Ext date pos attrs' content

pRecord :: Parser (Ext Record)
pRecord = try (ext pTemplate)
      <|> try (ext pRule)
      <|> try (ext pPeriodic)
      <|> try (ext pStop)
      <|> try (ext pSetRate)
      <|> ext (Transaction <$> pTransaction pAmount)

pTransaction :: Parser v -> Parser (Transaction v)
pTransaction p =
        try (pEntry p)
    <|> try (pReconciliate p)
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
  path <- pPath
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

pEntry :: Parser v -> Parser (Transaction v)
pEntry p = do
  es <- many1 (try (Left <$> pCreditPosting p) <|> (try (Right <$> pDebitPosting p)))
  corr <- optionMaybe $ do
            spaces
            x <- pPath <?> "corresponding account path"
            return x
  let cr = lefts es
      dt = rights es
  account <- case corr of
               Nothing -> return Nothing
               Just path -> Just <$> getAccount getPosition (getCoA <$> getState) path
  return $ TEntry $ UEntry dt cr account []

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
  path <- pPath
  account <- getAccount getPosition (getCoA <$> getState) path 
  spaces
  x <- p
  return $ TReconciliate account x

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

pCreditPosting :: Parser v -> Parser (Posting v Credit)
pCreditPosting p = do
  spaces
  reserved "cr"
  accPath <- pPath
  acc <- getAccountT AGCredit getPosition (getCoA <$> getState) accPath
  account <- case acc of
               WFree   _ acc -> return $ Left acc
               WCredit _ acc -> return $ Right acc
               _ -> fail $ printf "Invalid account type: %s: debit instead of credit." (intercalate "/" accPath)
  spaces
  amount <- p
  many newline
  return $ CPosting account amount

pDebitPosting :: Parser v -> Parser (Posting v Debit)
pDebitPosting p = do
  spaces
  reserved "dr"
  accPath <- pPath
  acc <- getAccountT AGDebit getPosition (getCoA <$> getState) accPath
  account <- case acc of
               WFree   _ acc -> return $ Left acc
               WDebit _ acc -> return $ Right acc
               _ -> fail $ printf "Invalid account type: %s: credit instead of debit." (intercalate "/" accPath)
  spaces
  amount <- p
  many newline
  return $ DPosting account amount

pAmount :: Parser Amount
pAmount = try numberFirst <|> currencyFirst
  where
    numberFirst = do
      n <- number
      c <- currency
      return $ n :# c

    currencyFirst = do
      c <- currency
      n <- number
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
loadTransactions :: FilePath -> Currencies -> ChartOfAccounts -> FilePath -> IO [Ext Record]
loadTransactions _ currs coa path = do
  content <- readFile path
  st <- emptyPState coa currs
  case runParser pRecords st path content of
    Right res -> return res
    Left err -> fail $ show err

