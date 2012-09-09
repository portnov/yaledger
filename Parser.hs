
module Parser where

import Control.Applicative
import Data.Maybe
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

import Types
import Tree

-- The lexer
lexer       = P.makeTokenParser haskellDef    
    
parens      = P.parens lexer
braces      = P.braces lexer
identifier  = P.identifier lexer
symbol      = P.symbol lexer
reserved    = P.reserved lexer
reservedOp  = P.reservedOp lexer
comma       = P.comma lexer
semicolon   = P.semi lexer
stringLit   = P.stringLiteral lexer

data PState = PState {
    lastID :: Integer,
    groupCurrency :: Currency,
    groupType :: AccountGroupType }
  deriving (Eq, Show)

emptyPState :: PState
emptyPState = PState {
  lastID = 0,
  groupCurrency = "",
  groupType = AGFree }

type Parser a = Parsec String PState a

account :: AccountGroupType -> String -> Integer -> Currency -> Attributes -> AnyAccount
account AGDebit  name aid c attrs = WDebit  attrs $ DAccount name aid c []
account AGCredit name aid c attrs = WCredit attrs $ CAccount name aid c []
account AGFree   name aid c attrs = WFree   attrs $ FAccount name aid c [] []

newID :: Parser Integer
newID = do
  st <- getState
  let r = lastID st + 1
      st' = st {lastID = r}
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
  aid <- newID
  let mbCurrency = lookup "currency" attrs
      currency = fromMaybe (groupCurrency st) mbCurrency
  return $ account tp name aid currency attrs

pAttributes :: Parser Attributes
pAttributes = try attribute `sepEndBy` semicolon
  where
    attribute = do
      name <- identifier
      reservedOp "="
      value <- stringLit
      return (name, value)

pAccountGroup :: Parser AccountPlan 
pAccountGroup = do
  st <- getState
  symbol "group"
  name <- identifier
  tp <- pAGType (groupType st)
  reserved "{"
  attrs <- option [] pAttributes
  let currency = fromMaybe (groupCurrency st) $ lookup "currency" attrs
      agData r = AccountGroupData {
                   agName = name,
                   agRange = r,
                   agCurrency = currency,
                   agType = tp,
                   agAttributes = attrs }
  let st' = st {
              groupCurrency = currency,
              groupType = tp }
  putState st'
  accs <- pAccount `sepEndBy` semicolon
  groups <- pAccountGroup `sepEndBy` semicolon
  reserved "}"
  st1 <- getState
  let range = (lastID st, lastID st1)
  putState $ st {lastID = lastID st1}
  return $ branch name (agData range) (map mkLeaf accs ++ groups)

mkLeaf :: AnyAccount -> AccountPlan
mkLeaf acc = leaf (getName acc) acc

