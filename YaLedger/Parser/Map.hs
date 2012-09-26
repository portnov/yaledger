
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
pAccountMap = pMapEntry `sepEndBy1` many newline

pMapEntry :: Parser AMEntry
pMapEntry = do
  ptr <- try (AMAccount <$> pAccount)
     <|> try (AMGroup <$> pGroup)
     <|> (AMAttributes <$> pFromAttributes)
  spaces
  reservedOp "->"
  spaces
  target <- try pToAttributes <|> pToAccountPlan
  return $ ptr :=> target

pToAccountPlan :: Parser AMTo
pToAccountPlan = do
  tgtPath <- pPath
  ToAccountPlan <$> getAccountPlanItem accountPlan tgtPath

pToAttributes :: Parser AMTo
pToAttributes =
  ToAttributes <$> braces pAttributes

pAccount :: Parser AccountID
pAccount = do
  reserved "account"
  spaces
  path <- pPath
  getID <$> getAccount accountPlan path

pGroup :: Parser GroupID
pGroup = do
  reserved "group"
  spaces
  path <- pPath
  x <- getAccountPlanItem accountPlan path
  case x of
    Branch {branchData = ag} -> return (agID ag)
    _ -> fail $ "This is an account, not accounts group:" ++ intercalate "/" path

pFromAttributes :: Parser Attributes
pFromAttributes = do
  reserved "attributes"
  spaces
  braces pAttributes

