{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module YaLedger.Parser.Map where

import Control.Applicative ((<$>))
import Data.List
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text hiding (Parser)

import YaLedger.Types
import YaLedger.Kernel.Common
import YaLedger.Parser.Common

data PState = PState {
    getCoA :: ChartOfAccounts }
  deriving (Eq, Show)

type Parser a = Parsec Text PState a

instance CMonad (Parsec Text PState) where
   cGetPosition = getPosition
   cGetCoA = getCoA <$> getState

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
  ToCoA <$> getCoAItem tgtPath

pToAttributes :: Parser AMTo
pToAttributes =
  ToAttributes <$> braces pAttributes

pAccount :: Parser AccountID
pAccount = do
  reserved "account"
  spaces
  path <- pPath
  getID <$> getAccount path

pGroup :: Parser GroupID
pGroup = do
  reserved "group"
  spaces
  path <- pPath
  x <- getCoAItem path
  case x of
    Branch {branchData = ag} -> return (agID ag)
    _ -> fail $ "This is an account, not accounts group:" ++ intercalate "/" path

pFromAttributes :: Parser Attributes
pFromAttributes = do
  reserved "attributes"
  spaces
  braces pAttributes

