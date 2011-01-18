{-# LANGUAGE UnicodeSyntax #-}

module Parser where
  
import Text.ParserCombinators.Parsec hiding (spaces,newline)
import qualified Text.ParserCombinators.Parsec.Expr as PE 
import qualified Text.ParserCombinators.Parsec.Language as L 
import qualified Text.ParserCombinators.Parsec.Token as Tok 
 
import Types
import Unicode
import Dates
import qualified Tree as T

-- | One or more spaces
spaces ∷ MParser String
spaces = many1 □ oneOf " \t"

-- | Zero or more spaces
spaces0 ∷ MParser String
spaces0 = many □ oneOf " \t"

-- | Any word
word ∷ MParser String
word = many1 □ noneOf " \t\n\r;{}()"

newline ∷ MParser String
newline = many1 □ oneOf "\n\r"

localDef = L.haskellStyle {
              L.identStart = L.identLetter localDef,
              L.identLetter = noneOf " \t\r\n" }
lexer = Tok.makeTokenParser localDef
 
symbol = Tok.symbol lexer 

anySymbol = do
  w ← word
  skipMany □ oneOf " \t"
  return w

braces = Tok.braces lexer

separator :: MParser String
separator = many1 □ oneOf " \t\r\n"
  

maybeLink p = do
  x ← optionMaybe p
  case x of
    Nothing -> return NoLink
    Just a  -> return □ ByName a

p1 >>- p2 = do
  x ← p1
  p2
  return x

-- | Get currency which is currently set as default
getDefCurrency ∷ MParser Currency
getDefCurrency = do
  st ← getState
  let lst = defaultCurrencies st
  case lst of
    [] -> fail "Currency not specified and no default currency"
    (c:_) -> return c

-- | Set currency as default
setDefCurrency ∷ Currency -> MParser ()
setDefCurrency c = do
  st ← getState
  let lst = defaultCurrencies st
  setState □ st { defaultCurrencies = c: defaultCurrencies st }

-- | Pop previous default currency
popDefCurrency ∷ MParser ()
popDefCurrency = do
  st ← getState
  let lst = defaultCurrencies st
  case lst of
    [] -> error "Internal error: could not `pop` default currency"
    (_:cs) -> setState □ st { defaultCurrencies = cs }

-- | Currency symbol or nothing
optionalCurrency = do
  c' ← optionMaybe anySymbol
  case c' of
    Nothing -> getDefCurrency 
    Just c -> return c

-- | Fix all links in tree
convertTree ∷ AccountsTree -> AccountsTree
convertTree tree = lookupAll tree tree
  where
    lookupAll t (T.Node name c children) = T.Node name c □ map (lookupAll t) children
    lookupAll t (T.Leaf name (Account name' c from to hld hist)) = T.Leaf name □ Account name' c (lookup from t) (lookup to t) hld hist

    lookup NoLink _ = NoLink
    lookup (LinkTo to) _ = LinkTo to
    lookup (ByName n) tr = 
      case T.lookupPath n tr of
        [x] -> LinkTo x
        []   -> error □ "Unknown account: " ++ n
        _    -> error □ "Ambigous account spec: " ++ n

-- | Ledger source
ledgerSource ∷ MParser (AccountsTree, [Dated Record])
ledgerSource = do
  now ← getCurrentDateTime'
  let y = year now
  accs ← pGroup
  recs ← pRecords y >>- eof
  return (accs, recs)

pAccount ∷ MParser AccountsTree
pAccount = do
    spaces0
    symbol "@account"
    name ← anySymbol
    c ← optionalCurrency 
    i ← maybeLink incFrom
    o ← maybeLink outTo
    h ← option (0:#"") pHold
    return □ T.Leaf name □ Account name c i o h []
  where
    incFrom = do
      symbol "getFrom"
      x ← anySymbol
      return x
    outTo = do
      symbol "putTo"
      x ← anySymbol
      return x
    pHold = do
      symbol "hold"
      pAmount

pGroup ∷ MParser AccountsTree
pGroup = do
    spaces0
    symbol "@group"
    name ← anySymbol
    c  ← optionalCurrency 
    setDefCurrency c
    children ← braces □ (try pAccount <|> try pGroup) `sepEndBy` (symbol ";")
    popDefCurrency 
    return □ T.Node name c children

pAmount ∷ MParser Amount
pAmount = (try two) <|> one
  where
    one = do
      n ← pNumber
      c ← anySymbol
      return (n :# c)
    two = do
      c ← noneOf "0123456789 \t\n\r"
      n ← pNumber
      return (n :# [c])

-- | Amount parameter. Returns (param number, default amount)
pParam ∷ MParser (Int,Amount)
pParam = do
  char '#'
  n ← readM "parameter number" =<< many1 digit
  char '='
  a ← pAmount
  return (n-1, a)

-- | Amount or parameter.
pAmountParam ∷ MParser AmountParam
pAmountParam = (try onlyParam) <|>  (try paramPercent) <|> onlyAmount
  where
    onlyAmount = F `fmap` pAmount
    onlyParam = do
      (n,a) ← pParam
      return □ P 100 n a
    paramPercent = do
      p ← pNumber
      string "%("
      (n,a) ← pParam
      char ')'
      return □ P p n a

-- | One record
pRecord ∷ Int -> MParser Record
pRecord y = choice □ map try □ [
            PR `fmap` pTransaction,
            RR `fmap` pSetRate,
            RegR `fmap` pRegular,
            TR `fmap` pTemplate,
            mkVR `fmap` pVerify,
            mkCTR `fmap` pCallTemplate,
            mkHold `fmap` pHold,
            mkRuledP `fmap` pRuledP,
            mkRuledC `fmap` pRuledC]
  where
    mkCTR (name, args) = CTR name args
    mkRuledP (rw,rule,tr) = RuledP rw rule tr
    mkRuledC (rw,rule,name,args) = RuledC rw rule name args
    mkVR (name,a) = VR name a
    mkHold (name,a) = Hold name a

pRecords ∷ Int -> MParser [Dated Record]
pRecords y = many1 (dated □ pRecord y)

pTransaction ∷ MParser Transaction
pTransaction = do
  s ← noneOf "\n\r"
  descr ← many1 □ noneOf "\n\r"
  oneOf "\n\r"
  parts ← many1 pPosting
  return □ Transaction s descr parts

pPosting ∷ MParser Posting
pPosting = do
    spaces
    p ← (try concretePosting) <|> autoPosting
    newline
    return p
  where
    concretePosting = do
      name ← anySymbol
      a ← pAmountParam 
      return (name :<+ a)
    autoPosting = 
      Auto `fmap` anySymbol

dated ∷ MParser a -> MParser (Dated a)
dated parser = do
  dt ← pDateOnly 
  spaces
  a ← parser
  return (At dt a)

pSetRate ∷ MParser SetRate
pSetRate = do
  symbol "@rate"
  from ← anySymbol
  symbol "="
  to ← pAmount
  optional newline
  return □ from := to

pHold ∷ MParser (String, Amount)
pHold = do
  symbol "@hold"
  name ← anySymbol
  h ← pAmount
  optional newline
  return (name, h)

pVerify ∷ MParser (String,Amount)
pVerify = do
  symbol "@balance"
  name ← anySymbol
  val ← pAmount
  optional newline
  return (name, val)

pRegular ∷ MParser RegularTransaction
pRegular = do
  symbol "@regular"
  (start,int) ← pSeries 
  newline
  spaces
  tr ← pTransaction 
  return □ RegularTransaction start int tr

pTemplate ∷ MParser Template
pTemplate = do
  symbol "@template"
  name ← anySymbol
  newline
  tr ← pTransaction
  return □ Template name 1 tr

pCallTemplate ∷ MParser (String, [Amount])
pCallTemplate = do
  symbol "@call"
  name ← anySymbol
  lst ← pAmount `sepBy` spaces
  newline
  return (name, lst)

pRuleWhen ∷ MParser RuleWhen
pRuleWhen = (read  ∘ capitalize) `fmap` (symbol "before" <|> symbol "after")

pRule ∷ MParser Rule
pRule = (try descr) <|> (try □ cmp '<') <|> (try □ cmp '>')
  where
    descr = do
      string "description like"
      spaces
      regex ← anySymbol
      newline
      return □ DescrMatch regex
    cmp ch = do
        name ← anySymbol
        char ch
        spaces0
        a ← pAmount
        newline
        return □ name `op` a
      where
        op | ch ≡ '<' = (:<)
           | otherwise = (:>)

pRuledP ∷ MParser (RuleWhen, Rule, Transaction)
pRuledP = do
  symbol "@rule"
  rw ← pRuleWhen 
--   spaces
  rule ← pRule
--   newline
  tr ← pTransaction 
  return (rw, rule, tr)

pRuledC ∷ MParser (RuleWhen,Rule, String, [Amount])
pRuledC = do
  symbol "@ruleCall"
  rw ← pRuleWhen 
  spaces
  rule ← pRule
  spaces
  name ← anySymbol
  lst ← pAmount `sepBy` spaces
  return (rw, rule, name, lst)

pSign ∷ (Num a) ⇒ MParser a
pSign = do
  s ← optionMaybe □ oneOf "+-"
  return □ case s of
             Just '+' → 1
             Just '-' → -1
             Nothing → 1

pNumber ∷ MParser Double
pNumber = do
  sgn ← pSign
  m ← pMantiss
  e ← optionMaybe □ oneOf "eE"
  osgn ← pSign
  o ← if e ≡ Nothing
        then return "0"
        else many1 digit
  return □ sgn * m * 10^(osgn*(readE "order" o∷Int))

pMantiss ∷ MParser Double
pMantiss = do
  i ← readM "integer part" =<< many1 digit
  p ← optionMaybe □ oneOf ".,"
  m ← if p ≡ Nothing
        then return "0"
        else many digit
  let n = length m
  return □ i + (readE "frac part" m)/(10^n)
