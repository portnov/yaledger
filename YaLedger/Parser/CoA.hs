
module YaLedger.Parser.CoA where

import Control.Applicative ((<$>))
import Control.Monad.Trans
import Data.Maybe
import Data.Decimal
import Data.IORef
import qualified Data.Map as M
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

type Parser a = ParsecT String PState IO a

account :: AccountGroupType -> String -> Integer -> Currency -> BalanceChecks -> Attributes -> Parser AnyAccount
account AGDebit  name aid c checks attrs = do
    empty1 <- lift $ newIORef []
    empty2 <- lift $ newIORef []
    return $ WDebit  attrs $ DAccount name aid c checks empty1 empty2
account AGCredit name aid c checks attrs = do
    empty1 <- lift $ newIORef []
    empty2 <- lift $ newIORef []
    return $ WCredit attrs $ CAccount name aid c checks empty1 empty2
account AGFree   name aid c checks attrs = do
    empty1 <- lift $ newIORef []
    empty2 <- lift $ newIORef []
    empty3 <- lift $ newIORef []
    return $ WFree   attrs $ FAccount name aid c checks empty1 empty2 empty3

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

lookupCurrency :: Attributes -> Parser Currency
lookupCurrency attrs = do
  st <- getState
  mbCurrency <- case M.lookup "currency" attrs of
                  Nothing -> return Nothing
                  Just (Exactly c) -> return (Just c)
                  Just _ -> fail $ "Currency must be specified exactly!"
  return $ fromMaybe (groupCurrency st) mbCurrency

pAccount :: Parser AnyAccount
pAccount = do
  st <- getState
  symbol "account"
  name <- identifier
  tp <- pAGType (groupType st)
  (checks, attrs) <- option (noChecks, M.empty) $ braces $ pAllAttributes
  aid <- newAID
  currency <- lookupCurrency attrs
  account tp name aid currency checks attrs

pAllAttributes :: Parser (BalanceChecks, Attributes)
pAllAttributes = do
  infoC    <- optionMaybe (bcheck "info")
  warningC <- optionMaybe (bcheck "warning")
  errorC   <- optionMaybe (bcheck "error")
  attrs <- pAttributes
  return (BalanceChecks infoC warningC errorC, attrs)

bcheck :: String -> Parser Decimal
bcheck kind = do
  symbol kind
  spaces
  reservedOp "="
  res <- number
  semicolon
  return res

pAccountGroup :: Parser ChartOfAccounts 
pAccountGroup = do
  st <- getState
  symbol "group"
  name <- identifier
  tp <- pAGType (groupType st)
  gid <- newGID
  reserved "{"
  attrs <- option M.empty pAttributes
  currency <- lookupCurrency attrs
  let agData r = AccountGroupData {
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
  children <- (try (mkLeaf <$> pAccount) <|> pAccountGroup) `sepEndBy` semicolon
  reserved "}"
  st1 <- getState
  let range = (lastAID st, lastAID st1)
  putState $ st {lastAID = lastAID st1, lastGID = lastGID st1}
  return $ branch name (agData range) children

mkLeaf :: AnyAccount -> ChartOfAccounts
mkLeaf acc = leaf (getName acc) acc

