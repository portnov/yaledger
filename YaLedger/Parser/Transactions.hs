
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

pTransactions :: Parser [Ext Transaction]
pTransactions = pTransaction `sepEndBy1` (newline >> newline)

pTransaction :: Parser (Ext Transaction)
pTransaction = do
  char '@'
  space
  st <- getState
  date <- try $ pDate (currentDate st)
  descr <- optionMaybe $ many1 $ noneOf "\n\r"
  newline
  attrs <- option [] $ braces $ pAttributes
  tran <- pTran
  let attrs' = case descr of
                 Nothing -> attrs
                 Just s -> ("description", s): attrs
  return $ Ext date attrs' tran

pTran :: Parser Transaction
pTran = do
  es <- many1 $ try (try (Left <$> pCreditEntry) <|> (Right <$> pDebitEntry))
  corr <- optionMaybe $ try $ do
            newline
            spaces
            identifier
  let cr = lefts es
      dt = rights es
  account <- case corr of
               Nothing -> return Nothing
               Just path -> Just <$> getAccount path
  return $ TPosting $ UPosting dt cr account

getAccount :: String -> Parser AnyAccount
getAccount path = do
  st <- getState
  case lookupPath path (accountPlan st) of
    [] -> fail $ "No such account: " ++ path
    [a] -> return a
    as -> fail $ printf "Ambigous account specification: %s (%d matching accounts)."
                        path
                        (length as)

pCreditEntry :: Parser (Entry Credit)
pCreditEntry = do
  spaces
  symbol "cr"
  accPath <- identifier
  acc <- getAccount accPath
  account <- case acc of
               WFree   _ acc -> return $ Left acc
               WCredit _ acc -> return $ Right acc
               _ -> fail $ printf "Invalid account type: %s: debit instead of credit." accPath
  spaces
  amount <- pAmount
  return $ CEntry account amount

pDebitEntry :: Parser (Entry Debit)
pDebitEntry = do
  spaces
  symbol "dr"
  accPath <- identifier
  acc <- getAccount accPath
  account <- case acc of
               WFree   _ acc -> return $ Left acc
               WDebit _ acc -> return $ Right acc
               _ -> fail $ printf "Invalid account type: %s: credit instead of debit." accPath
  spaces
  amount <- pAmount
  return $ DEntry account amount

pAmount :: Parser Amount
pAmount = do
  ns <- float
  c <- many $ noneOf " \r\n\t"
  return $ realFracToDecimal 10 ns :# c

