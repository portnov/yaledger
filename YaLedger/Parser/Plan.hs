
module YaLedger.Parser.Plan where

import Control.Applicative
import Data.Maybe
import Text.Parsec

import YaLedger.Types
import YaLedger.Tree
import YaLedger.Parser.Common

data PState = PState {
    lastAID :: Integer,
    lastGID :: Integer,
    groupCurrency :: Currency,
    groupType :: AccountGroupType }
  deriving (Eq, Show)

emptyPState :: PState
emptyPState = PState {
  lastAID = 0,
  lastGID = 0,
  groupCurrency = "",
  groupType = AGFree }

type Parser a = Parsec String PState a

account :: AccountGroupType -> String -> Integer -> Currency -> Attributes -> AnyAccount
account AGDebit  name aid c attrs = WDebit  attrs $ DAccount name aid c []
account AGCredit name aid c attrs = WCredit attrs $ CAccount name aid c []
account AGFree   name aid c attrs = WFree   attrs $ FAccount name aid c [] []

newAID :: Parser Integer
newAID = do
  st <- getState
  let r = lastAID st + 1
      st' = st {lastAID = r}
  putState st'
  return r

newGID :: Parser Integer
newGID = do
  st <- getState
  let r = lastGID st + 1
      st' = st {lastGID = r}
  putState st'
  return r

pAGType :: AccountGroupType -> Parser AccountGroupType
pAGType AGFree = do
  st <- getState
  t <- optionMaybe $ parens identifier
  case t of
    Just "debit"  -> return $ AGDebit
    Just "credit" -> return $ AGCredit
    Just "free"   -> return $ AGFree
    Just x        -> fail $ "Unknown account type: " ++ x
    Nothing       -> return AGFree
pAGType t = do
    notFollowedBy (parens identifier) <?> ("Cannot override account type: " ++ show t)
    return t

pAccount :: Parser AnyAccount
pAccount = do
  st <- getState
  symbol "account"
  name <- identifier
  tp <- pAGType (groupType st)
  attrs <- option [] $ braces $ pAttributes
  aid <- newAID
  let mbCurrency = lookup "currency" attrs
      currency = fromMaybe (groupCurrency st) mbCurrency
  return $ account tp name aid currency attrs

pAccountGroup :: Parser AccountPlan 
pAccountGroup = do
  st <- getState
  symbol "group"
  name <- identifier
  tp <- pAGType (groupType st)
  gid <- newGID
  reserved "{"
  attrs <- option [] pAttributes
  let currency = fromMaybe (groupCurrency st) $ lookup "currency" attrs
      agData r = AccountGroupData {
                   agName = name,
                   agID = gid,
                   agRange = r,
                   agCurrency = currency,
                   agType = tp,
                   agAttributes = attrs }
  let st' = st {
              lastGID = gid,
              groupCurrency = currency,
              groupType = tp }
  putState st'
  accs <- pAccount `sepEndBy` semicolon
  groups <- pAccountGroup `sepEndBy` semicolon
  reserved "}"
  st1 <- getState
  let range = (lastAID st, lastAID st1)
  putState $ st {lastAID = lastAID st1, lastGID = lastGID st1}
  return $ branch name (agData range) (map mkLeaf accs ++ groups)

mkLeaf :: AnyAccount -> AccountPlan
mkLeaf acc = leaf (getName acc) acc

