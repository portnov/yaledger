
module Main where

import Control.Monad
import Data.Dates hiding (pDate)
import Data.List.Utils
import Text.Parsec
import Text.Parsec.String
import Text.Regex.PCRE

import YaLedger.Types
import YaLedger.Tree
import YaLedger.Pretty
import YaLedger.Parser

pDate :: Parsec String () DateTime
pDate = do
  dayS <- many1 digit
  char '/'
  monthS <- many1 digit
  char '/'
  yearS <- many1 digit
  return $ DateTime (read yearS) (read monthS) (read dayS) 0 0 0

accountName = "карточка"
currency = "р"

categories =
  [("ИНТЕРНЕТ", "интернет"),
   ("премия",   "премия"),
   ("Заработная плата", "зарплата"),
   ("Аванс", "аванс"),
   ("Выдача наличных", "наличные"),
   ("Комиссия", "комиссии"),
   ("Плата за обслуживание", "комиссии")]

getCategory :: String -> Maybe String
getCategory description =
    case concatMap check categories of
      [] -> Nothing
      list -> Just $ head list
  where
    check (regexp, category) =
      if description =~ regexp
        then [category]
        else []

readSum str = read $ filter (/= ' ') str

convert :: AccountPlan -> [String] -> IO (Ext Record)
convert plan row = do
  let dateStr     = row !! 0
      description = row !! 2
      amountStr   = row !! 3
      attrs = [("description", description)] ++
              case getCategory description of
                Nothing -> []
                Just category -> [("category", category)]

  (s, amount) <- case head amountStr of
                   '+' -> return (ECredit, readSum (tail amountStr))
                   '-' -> return (EDebit,  readSum (tail amountStr))
                   c   -> fail $ "Unknown amount sign: " ++ [c]
  date <- case runParser pDate () "<stdin>" dateStr of
            Right x -> return x
            Left err -> fail $ show err

  acc <- case lookupTree (mkPath accountName) plan of
           [] -> fail $ "No such account: " ++ accountName
           [a] -> return a
           as -> fail "Ambigous account specification"

  entry <- case s of
             ECredit -> do
               posting <- cposting acc (amount :# currency)
               return $ UEntry [] [posting] Nothing []
             EDebit -> do
               posting <- dposting acc (amount :# currency)
               return $ UEntry [posting] [] Nothing []
  return $ Ext date attrs (Transaction $ TEntry entry)

cposting :: AnyAccount -> Amount -> IO (Posting Amount Credit)
cposting acc x =
  case acc of
    WCredit _ a -> return $ CPosting (Right a) x
    WFree   _ a -> return $ CPosting (Left  a) x
    WDebit  _ _ -> fail $ "Invalid account type: debit instead of credit"

dposting :: AnyAccount -> Amount -> IO (Posting Amount Debit)
dposting acc x =
  case acc of
    WDebit  _ a -> return $ DPosting (Right a) x
    WFree   _ a -> return $ DPosting (Left  a) x
    WCredit  _ _ -> fail $ "Invalid account type: credit instead of debit"

parseRow :: String -> [String]
parseRow line = split ";" line

main = do
  plan <- readPlan "test.accounts"
  string <- getContents
  let csv = map parseRow $ lines string
  forM_ csv $ \row -> do
    record <- convert plan row
    putStrLn $ prettyPrint record


