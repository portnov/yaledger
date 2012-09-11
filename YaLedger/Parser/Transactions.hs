
module YaLedger.Parser.Transactions where

import Control.Applicative hiding (many, (<|>), optional)
import Data.Either
import Data.Decimal
import Data.Maybe
import Data.Dates
import Text.Parsec
import Text.Printf

import YaLedger.Types
import YaLedger.Tree
import YaLedger.Parser.Common

import Debug.Trace

data PState = PState {
  accountPlan :: AccountPlan,
  currentDate :: DateTime
  }

type Parser a = Parsec String PState a

emptyPState :: AccountPlan -> IO PState
emptyPState plan = do
  now <- getCurrentDateTime
  return $ PState {
             accountPlan = plan,
             currentDate = now }

pRecords :: Parser [Ext Record]
pRecords = do
  rs <- pRecord `sepEndBy1` (many newline <?> "N2")
  eof
  return rs

ext :: Parser a -> Parser (Ext a)
ext p = do
  char '@'
  space
  st <- getState
  date <- try $ pDate (currentDate st)
  descr <- optionMaybe $ many1 $ noneOf "\n\r"
  newline
  attrs <- option [] $ braces $ pAttributes
  let attrs' = case descr of
                 Nothing -> attrs
                 Just s -> ("description", s): attrs
  content <- p
  return $ Ext date attrs' content

pRecord :: Parser (Ext Record)
pRecord = try (ext pTemplate)
          <|> ext (Transaction <$> pTran number)

pTemplate :: Parser Record
pTemplate = do
  symbol "template"
  name <- identifier
  tran <- pTran param
  return $ Template name tran

pTran :: Parser v -> Parser (Transaction v)
pTran p = do
  es <- try (try (Left <$> pCreditPosting p) <|> (try (Right <$> pDebitPosting p))) `sepEndBy1` (newline <?> "N1")
  corr <- optionMaybe $ try $ do
            spaces
            x <- identifier <?> "I1"
            optional newline
            return x
  let cr = lefts es
      dt = rights es
  account <- case corr of
               Nothing -> return Nothing
               Just path -> Just <$> getAccount accountPlan (mkPath path)
  return $ TEntry $ UEntry dt cr account

pCreditPosting :: Parser v -> Parser (Posting v Credit)
pCreditPosting p = do
  spaces
  symbol "cr"
  accPath <- identifier
  acc <- getAccount accountPlan (mkPath accPath)
  account <- case acc of
               WFree   _ acc -> return $ Left acc
               WCredit _ acc -> return $ Right acc
               _ -> fail $ printf "Invalid account type: %s: debit instead of credit." accPath
  spaces
  amount <- pAmount p
  return $ CPosting account amount

pDebitPosting :: Parser v -> Parser (Posting v Debit)
pDebitPosting p = do
  spaces
  symbol "dr"
  accPath <- identifier
  acc <- getAccount accountPlan (mkPath accPath)
  account <- case acc of
               WFree   _ acc -> return $ Left acc
               WDebit _ acc -> return $ Right acc
               _ -> fail $ printf "Invalid account type: %s: credit instead of debit." accPath
  spaces
  amount <- pAmount p
  return $ DPosting account amount

pAmount :: Parser v -> Parser (Amount v)
pAmount p = do
  n <- p
  c <- many $ noneOf " \r\n\t"
  return $ n :# c

number :: Parser Decimal
number = do
  x <- float
  return $ realFracToDecimal 10 x

param :: Parser Param
param = do
  char '#'
  ns <- many1 digit
  let n = read ns
  a <- optionMaybe $ try $ reservedOp "*"
  c <- case a of
         Nothing -> return 1.0
         Just _ -> float
  spaces
  d <- option 0 $ try $ parens $ do
           symbol "default"
           number
  return $ Param n c d

