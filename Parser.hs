{-# LANGUAGE UnicodeSyntax #-}

module Parser where
  
import Text.ParserCombinators.Parsec

import Types
import Dates

pAmount :: Parser Amount
pAmount = (try two) <|> one
  where
    one = do
      n <- pNumber
      c <- many1 $ noneOf " \t\n\r"
      return (n :# c)
    two = do
      c <- noneOf "0123456789 \t\n\r"
      n <- pNumber
      return (n :# [c])

pParam :: Parser (Int,Amount)
pParam = do
  char '#'
  n <- (readM "parameter number") =<< (many1 digit)
  char '='
  a <- pAmount
  return (n,a)

pAmountParam :: Parser AmountParam
pAmountParam = (try onlyParam) <|>  (try paramPercent) <|> onlyAmount
  where
    onlyAmount = F `fmap` pAmount
    onlyParam = do
      (n,a) <- pParam
      return $ P 100 n a
    paramPercent = do
      p <- pNumber
      string "%("
      (n,a) <- pParam
      char ')'
      return $ P p n a

pRecord :: Int -> Parser Record
pRecord y = choice $ map try $ [
            PR `fmap` pTransaction,
            RR `fmap` pSetRate,
            RegR `fmap` pRegular y,
            TR `fmap` pTemplate,
            mkCTR `fmap` pCallTemplate,
            mkRuledP `fmap` pRuledP,
            mkRuledC `fmap` pRuledC]
  where
    mkCTR (name, args) = CTR name args
    mkRuledP (rw,rule,tr) = RuledP rw rule tr
    mkRuledC (rw,rule,name,args) = RuledC rw rule name args

pRecords :: Int -> Parser [Dated Record]
pRecords y = (dated y $ pRecord y) `sepBy` (many1 $ oneOf "\n\r")

pTransaction :: Parser Transaction
pTransaction = do
  s <- noneOf "\n\r"
  many1 $ oneOf " \t"
  descr <- many1 $ noneOf "\n\r"
  oneOf "\n\r"
  parts <- many1 pPosting
  return $ Transaction s descr parts

pPosting :: Parser Posting
pPosting = do
    many1 $ oneOf " \t"
    p <- (try concretePosting) <|> autoPosting
    oneOf "\n\r"
    return p
  where
    concretePosting = do
      name <- many1 $ noneOf " \t\n\r"
      many1 $ oneOf " \t"
      a <- pAmountParam 
      return (name :<+ a)
    autoPosting = 
      Auto `fmap` (many1 $ noneOf " \t\n\r")

dated :: Int -> Parser a -> Parser (Dated a)
dated year parser = do
  dt <- pDateOnly year
  many1 $ oneOf " \t"
  a <- parser
  return (At dt a)

pSetRate :: Parser SetRate
pSetRate = do
  string "#rate"
  many1 $ oneOf " \t"
  from <- many1 $ noneOf " \t\r\n"
  string " = "
  to <- pAmount
  return $ from := to

pVerify :: Parser (String,Amount)
pVerify = do
  string "#balance"
  many1 $ oneOf " \t"
  name <- many1 $ noneOf " \t\r\n"
  many1 $ oneOf " \t"
  val <- pAmount
  return (name, val)

pRegular :: Int -> Parser RegularTransaction
pRegular year = do
  string "#regular"
  many1 $ oneOf " \t"
  (start,int) <- pSeries year
  oneOf "\n\r"
  many $ oneOf " \t"
  tr <- pTransaction 
  return $ RegularTransaction start int tr

pTemplate :: Parser Template
pTemplate = do
  string "#template"
  many1 $ oneOf " \t"
  name <- many1 $ noneOf " \t\r\n"
  oneOf "\n\r"
  tr <- pTransaction
  return $ Template name 1 tr

pCallTemplate :: Parser (String, [Amount])
pCallTemplate = do
  string "#call"
  many1 $ oneOf " \t"
  name <- many1 $ noneOf " \t\r\n"
  lst <- pAmount `sepBy` (many1 $ oneOf " \t")
  oneOf "\n\r"
  return (name, lst)

pRuleWhen :: Parser RuleWhen
pRuleWhen = (read . capitalize) `fmap` (string "before" <|> string "after")

pRule :: Parser Rule
pRule = (try descr) <|> (try $ cmp '<') <|> (try $ cmp '>')
  where
    descr = do
      string "description like"
      many1 $ oneOf " \t"
      regex <- many1 $ noneOf " \t\r\n"
      oneOf "\n\r"
      return $ DescrMatch regex
    cmp ch = do
        name <- many1 $ noneOf " \t\r\n"
        many $ oneOf " \t"
        char ch
        many $ oneOf " \t"
        a <- pAmount
        oneOf "\n\r"
        return $ name `op` a
      where
        op | ch == '<' = (:<)
           | otherwise = (:>)


pRuledP :: Parser (RuleWhen, Rule, Transaction)
pRuledP = do
  string "#rule"
  many1 $ oneOf " \t"
  rw <- pRuleWhen 
  many1 $ oneOf " \t"
  rule <- pRule
  oneOf "\n\r"
  tr <- pTransaction 
  return (rw, rule, tr)

pRuledC :: Parser (RuleWhen,Rule, String, [Amount])
pRuledC = do
  string "#ruleCall"
  many1 $ oneOf " \t"
  rw <- pRuleWhen 
  many1 $ oneOf " \t"
  rule <- pRule
  many1 $ oneOf " \t"
  name <- many1 $ noneOf " \t\r\n"
  many1 $ oneOf " \t"
  lst <- pAmount `sepBy` (many1 $ oneOf " \t")
  return (rw, rule, name, lst)

pSign ∷ (Num a) ⇒ Parser a
pSign = do
  s ← optionMaybe $ oneOf "+-"
  return $ case s of
             Just '+' → 1
             Just '-' → -1
             Nothing → 1

pNumber ∷ Parser Double
pNumber = do
  sgn ← pSign
  m ← pMantiss
  e ← optionMaybe $ oneOf "eE"
  osgn ← pSign
  o ← if e == Nothing
        then return "0"
        else many1 digit
  return $ sgn * m * 10^(osgn*(readE "order" o∷Int))

pMantiss ∷ Parser Double
pMantiss = do
  i ← readM "integer part" =<< many1 digit
  p ← optionMaybe $ oneOf ".,"
  m ← if p == Nothing
        then return "0"
        else many digit
  let n = length m
  return $ i + (readE "frac part" m)/(10^n)
