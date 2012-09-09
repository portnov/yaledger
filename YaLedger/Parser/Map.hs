
module YaLedger.Parser.Map where

import Control.Applicative ((<$>))
import Data.Maybe
import Data.List
import Text.Parsec
import Text.Printf

import YaLedger.Types
import YaLedger.Tree
import YaLedger.Parser.Common

data PState = PState {
    accountPlan :: AccountPlan }
  deriving (Eq, Show)

type Parser a = Parsec String PState a

pAccountMap :: Parser AccountMap
pAccountMap = pMapEntry `sepEndBy1` newline

pMapEntry :: Parser AMEntry
pMapEntry = do
  ptr <- try (AMAccount <$> pAccount) <|> (AMGroup <$> pGroup)
  spaces
  reservedOp "->"
  spaces
  tgtPath <- pPath
  target <- getAccount accountPlan tgtPath
  return $ ptr :=> target

pAccount :: Parser Integer
pAccount = do
  symbol "account"
  spaces
  path <- pPath
  account <- getAccount accountPlan path
  return $ getID account

pGroup :: Parser Integer
pGroup = do
  symbol "group"
  spaces
  path <- pPath
  st <- getState
  case search (accountPlan st) path of
    [] -> fail $ "No such account group: " ++ intercalate "/" path
    [Left ag] -> return (agID ag)
    xs ->  fail $ printf "Ambigous accounts group specification: %s (%d matching elements)."
                        (intercalate "/" path)
                        (length xs)

