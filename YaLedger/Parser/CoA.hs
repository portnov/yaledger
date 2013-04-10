
module YaLedger.Parser.CoA where

import Control.Applicative ((<$>))
import Control.Monad.Trans
import Control.Concurrent.STM
import Data.Maybe
import Data.Decimal
import qualified Data.Map as M
import Text.Parsec

import YaLedger.Types
import YaLedger.Parser.Common
import YaLedger.Kernel.Balances

data PState = PState {
    lastAID :: Integer,
    lastGID :: Integer,
    getThousandsSeparator :: Char,
    getDecimalSeparator :: Char, 
    declaredCurrencies :: Currencies,
    groupAttributes :: Attributes,
    groupCurrency   :: Currency,
    groupType       :: AccountGroupType }
  deriving (Eq, Show)

emptyPState :: Currencies -> PState
emptyPState currs = PState {
  lastAID = 0,
  lastGID = 0,
  getThousandsSeparator = ' ',
  getDecimalSeparator = '.',
  declaredCurrencies = currs,
  groupAttributes = M.empty,
  groupCurrency = emptyCurrency,
  groupType = AGFree }

type Parser a = ParsecT String PState IO a

account :: AccountGroupType -> String -> Integer -> Currency -> Bool -> BalanceChecks -> Attributes -> Parser AnyAccount
account AGDebit  name aid c _ checks attrs = do
    empty1 <- lift $ newTVarIO []
    empty2 <- lift $ newTVarIO []
    empty3 <- lift $ newTVarIO []
    return $ WDebit  $ DAccount name aid c attrs checks empty1 empty2 empty3
account AGCredit name aid c _ checks attrs = do
    empty1 <- lift $ newTVarIO []
    empty2 <- lift $ newTVarIO []
    empty3 <- lift $ newTVarIO []
    return $ WCredit $ CAccount name aid c attrs checks empty1 empty2 empty3
account AGFree   name aid c redirect checks attrs = do
    empty1 <- lift $ newTVarIO []
    empty2 <- lift $ newTVarIO []
    empty3 <- lift $ newTVarIO []
    empty4 <- lift $ newTVarIO []
    empty5 <- lift $ newTVarIO []
    return $ WFree   $ FAccount name aid c attrs redirect checks empty1 empty2 empty3 empty4 empty5

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
  t <- optionMaybe $ parens $ many1 letter
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
                  Just (Exactly s) -> do
                      case M.lookup s (declaredCurrencies st) of
                        Nothing -> fail $ "Unknown currency: " ++ s
                        Just c -> return (Just c)
                  Just _ -> fail $ "Currency must be specified exactly!"
  return $ fromMaybe (groupCurrency st) mbCurrency

pAccount :: Parser AnyAccount
pAccount = do
  st <- getState
  symbol "account"
  name <- identifier
  tp <- pAGType (groupType st)
  let parentAttrs = groupAttributes st
  (redirect, checks, attrs) <- option (False, noChecks, M.empty) $ braces $ pAllAttributes
  aid <- newAID
  currency <- lookupCurrency attrs
  account tp name aid currency redirect checks (attrs `M.union` parentAttrs)

pAllAttributes :: Parser (Bool, BalanceChecks, Attributes)
pAllAttributes = do
  redirect <- option False $ do
                reserved "redirect"
                semicolon
                return True
  infoC    <- optionMaybe (bcheck "info")
  warningC <- optionMaybe (bcheck "warning")
  errorC   <- optionMaybe (bcheck "error")
  attrs <- pAttributes
  return (redirect, BalanceChecks infoC warningC errorC, attrs)

bcheck :: String -> Parser Decimal
bcheck kind = do
  symbol kind
  spaces
  reservedOp "="
  res <- number getThousandsSeparator getDecimalSeparator
  semicolon
  return res

pAccountGroup :: Parser ChartOfAccounts 
pAccountGroup = do
  st <- getState
  symbol "group"
  name <- identifier
  let parentAttrs = groupAttributes st
  tp <- pAGType (groupType st)
  gid <- newGID
  reserved "{"
  attrs <- option M.empty pAttributes
  let allAttrs = attrs `M.union` parentAttrs
  currency <- lookupCurrency attrs
  let agData r = AccountGroupData {
                   agName = name,
                   agID = gid,
                   agRange = r,
                   agCurrency = currency,
                   agType = tp,
                   agAttributes = allAttrs }
  putState $ st {
              lastGID = gid,
              groupAttributes = allAttrs,
              groupCurrency = currency,
              groupType = tp }
  children <- (try (mkLeaf <$> pAccount) <|> pAccountGroup) `sepEndBy` semicolon
  reserved "}"
  st1 <- getState
  let range = (lastAID st, lastAID st1)
  putState $ st {lastAID = lastAID st1, lastGID = lastGID st1}
  return $ branch name (agData range) children

mkLeaf :: AnyAccount -> ChartOfAccounts
mkLeaf acc = leaf (getName acc) acc

