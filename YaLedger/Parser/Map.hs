module YaLedger.Parser.Map where

import Control.Applicative ((<$>))
import Data.List
import Text.Parsec

import YaLedger.Types
import YaLedger.Kernel.Common
import YaLedger.Parser.Common

data PState = PState {
    getCoA :: ChartOfAccounts }
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
  target <- try pToAttributes <|> pToCoA
  return $ ptr :=> target

pToCoA :: Parser AMTo
pToCoA = do
  tgtPath <- pPath
  ToCoA <$> getCoAItem getPosition (getCoA <$> getState) tgtPath

pToAttributes :: Parser AMTo
pToAttributes =
  ToAttributes <$> braces pAttributes

pAccount :: Parser AccountID
pAccount = do
  reserved "account"
  spaces
  path <- pPath
  getID <$> getAccount getPosition (getCoA <$> getState) path

pGroup :: Parser GroupID
pGroup = do
  reserved "group"
  spaces
  path <- pPath
  x <- getCoAItem getPosition (getCoA <$> getState) path
  case x of
    Branch {branchData = ag} -> return (agID ag)
    _ -> fail $ "This is an account, not accounts group:" ++ intercalate "/" path

pFromAttributes :: Parser Attributes
pFromAttributes = do
  reserved "attributes"
  spaces
  braces pAttributes

