
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
    groupCurrency :: Currency,
    groupType :: AccountGroupType }
  deriving (Eq, Show)

emptyPState :: PState
emptyPState = PState {
  groupCurrency = "",
  groupType = AGFree }

type Parser a = Parsec String PState a

account :: AccountGroupType -> String -> Currency -> Attributes -> AnyAccount
account AGDebit  name c attrs = WDebit  attrs $ DAccount name c []
account AGCredit name c attrs = WCredit attrs $ CAccount name c []
account AGFree   name c attrs = WFree   attrs $ FAccount name c [] []

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
  let mbCurrency = lookup "currency" attrs
      currency = fromMaybe (groupCurrency st) mbCurrency
  return $ account tp name currency attrs

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
      agData = AccountGroupData {
                 agName = name,
                 agCurrency = currency,
                 agType = tp,
                 agAttributes = attrs }
  let st' = PState {
              groupCurrency = currency,
              groupType = tp }
  putState st'
  accs <- pAccount `sepEndBy` semicolon
  groups <- pAccountGroup `sepEndBy` semicolon
  reserved "}"
  putState st
  return $ branch name agData (map mkLeaf accs ++ groups)

mkLeaf :: AnyAccount -> AccountPlan
mkLeaf acc = leaf (getName acc) acc

