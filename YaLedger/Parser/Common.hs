{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module YaLedger.Parser.Common where

import Control.Applicative ((<$>))
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Printf

import YaLedger.Types

import Debug.Trace

language :: (Stream s m Char, Monad m) => GenLanguageDef s u m
language    = P.LanguageDef
               { P.commentStart   = ""
               , P.commentEnd     = ""
               , P.commentLine    = "--"
               , P.nestedComments = True
               , P.identStart     = letter <|> char '_'
               , P.identLetter    = alphaNum <|> oneOf "_'"
               , P.opStart        = P.opLetter language
               , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , P.reservedOpNames= ["account", "group", "template",
                                   "call", "reconciliate", "rate",
                                   "cr", "dr", "default"]
               , P.reservedNames  = []
               , P.caseSensitive  = True
               }

-- The lexer
lexer       = P.makeTokenParser language
    
parens      = P.parens lexer
braces      = P.braces lexer
identifier  = P.identifier lexer
symbol      = P.symbol lexer
reserved    = P.reserved lexer
reservedOp  = P.reservedOp lexer
comma       = P.comma lexer
semicolon   = P.semi lexer
stringLit   = P.stringLiteral lexer
float       = P.float lexer

pRegexp :: Monad m => ParsecT String st m String
pRegexp = do
    char '/'
    go ""
  where
    go acc = do
      c <- anyChar
      v <- case c of
            '\\' -> anyChar
            _    -> return c
      if v == '/'
        then return acc
        else go (acc ++ [v])

pAttributeValue :: Monad m => ParsecT String st m AttributeValue
pAttributeValue =
        try (Exactly <$> stringLit)
    <|> try (AnyBut  <$> anyBut)
    <|> (Regexp <$> pRegexp)
  where
    anyBut = do
      char '!'
      stringLit

pAttributes :: Monad m => ParsecT String st m Attributes
pAttributes = M.fromList <$> try attribute `sepEndBy` semicolon
  where
    attribute = do
      name <- identifier
      reservedOp "="
      value <- pAttributeValue
      return (name, value)

pPath :: Monad m => ParsecT String st m Path
pPath = identifier `sepBy` reservedOp "/"

getAccount :: Monad m => (st -> AccountPlan) -> Path -> ParsecT String st m AnyAccount
getAccount accountPlan path = do
  st <- getState
  case lookupTree path (accountPlan st) of
    [] -> fail $ "No such account: " ++ intercalate "/" path
    [a] -> return a
    as -> fail $ printf "Ambigous account specification: %s (%d matching accounts)."
                        (intercalate "/" path)
                        (length as)

getAccountPlanItem :: Monad m => (st -> AccountPlan) -> Path -> ParsecT String st m AccountPlan
getAccountPlanItem accountPlan path = do
  st <- getState
  case search' (accountPlan st) path of
    [] -> fail $ "No such account plan item: " ++ intercalate "/" path
    [a] -> return a
    as -> fail $ printf "Ambigous account plan item specification: %s (%d matching items)."
                        (intercalate "/" path)
                        (length as)

