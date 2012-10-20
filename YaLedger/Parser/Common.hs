{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module YaLedger.Parser.Common where

import Control.Applicative ((<$>))
import Control.Failure
import Control.Exception hiding (try)
import Data.Functor.Identity
import Data.Decimal
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Yaml
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import System.FilePath
import System.Environment.XDG.BaseDir

import YaLedger.Types

instance Exception e => Failure e Identity where
  failure e = fail $ show e

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
                                   "credit", "debit", "when", "do",
                                   "rule", "cr", "dr", "default"]
               , P.reservedNames  = []
               , P.caseSensitive  = True
               }

-- The lexer
lexer       = P.makeTokenParser language
    
parens      = P.parens lexer
braces      = P.braces lexer
brackets    = P.brackets lexer
identifier  = P.identifier lexer
symbol      = P.symbol lexer
reserved    = P.reserved lexer
reservedOp  = P.reservedOp lexer
comma       = P.comma lexer
semicolon   = P.semi lexer
stringLit   = P.stringLiteral lexer
float       = P.float lexer
natural     = P.natural lexer
naturalOrFloat = P.naturalOrFloat lexer

number :: Monad m => ParsecT String st m Decimal
number = do
  x <- naturalOrFloat
  return $ case x of
             Left i  -> realFracToDecimal 10 (fromIntegral i)
             Right x -> realFracToDecimal 10 x

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
    <|> try (Optional <$> pOptional)
    <|> try (AnyBut  <$> anyBut)
    <|> try (OneOf   <$> brackets list)
    <|> try (reservedOp "*" >> return Any)
    <|> (Regexp <$> pRegexp)
  where
    anyBut = do
      char '!'
      stringLit

    pOptional = do
      char '?'
      stringLit

    list = stringLit `sepBy1` comma

pAttributes :: Monad m => ParsecT String st m Attributes
pAttributes = M.fromList <$> try pAttribute `sepEndBy` semicolon

pAttribute :: Monad m => ParsecT String st m (String, AttributeValue)
pAttribute = do
  name <- identifier
  reservedOp "="
  value <- pAttributeValue
  return (name, value)

pPath :: Monad m => ParsecT String st m Path
pPath = try identifier `sepBy1` reservedOp "/"

currencySymbol :: Monad m => ParsecT String st m String
currencySymbol =
  (many $ noneOf " \r\n\t\")}->@0123456789.") <?> "Currency symbol"

-- | Load a config from YAML file
loadParserConfig :: FromJSON config => FilePath -> IO config
loadParserConfig path = do
  fullPath <- case head path of
                '/' -> return path
                '.' -> return path
                _ -> do
                     configDir <- getUserConfigDir "yaledger"
                     return (configDir </> path)
  str <- B.readFile fullPath
  case decodeEither str of
    Left err -> fail $ "Cannot parse config file " ++ fullPath ++ ": " ++ err
    Right pc -> return pc

