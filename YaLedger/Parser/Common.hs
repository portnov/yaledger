{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}

module YaLedger.Parser.Common where

import Control.Applicative ((<$>))
import Control.Failure
import Control.Exception hiding (try)
import Data.Char (ord)
import Data.List (isPrefixOf)
import Data.Functor.Identity
import Data.Decimal
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Yaml
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import System.FilePath
import System.Environment.XDG.BaseDir

import YaLedger.Types
import YaLedger.Parser.HTTP

instance Exception e => Failure e Identity where
  failure e = fail $ show e

toLazyByteString :: String -> L.ByteString
toLazyByteString str = L.pack $ map (fromIntegral . ord) str

readUrl :: String -> IO String
readUrl str =
  if "http://" `isPrefixOf` str
    then loadHTTP str
    else readFile str

readUrlLBS :: String -> IO L.ByteString
readUrlLBS str =
  if "http://" `isPrefixOf` str
    then toLazyByteString <$> loadHTTP str
    else L.readFile str

readUrlText :: String -> IO Text
readUrlText str =
  if "http://" `isPrefixOf` str
    then loadHTTPText str
    else TIO.readFile str

language :: (Stream s m Char, Monad m) => GenLanguageDef s u m
language    = P.LanguageDef
               { P.commentStart   = "{-"
               , P.commentEnd     = "-}"
               , P.commentLine    = "--"
               , P.nestedComments = True
               , P.identStart     = letter <|> char '_'
               , P.identLetter    = alphaNum <|> oneOf "_'"
               , P.opStart        = P.opLetter language
               , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , P.reservedNames= ["account", "group", "template",
                                   "call", "reconciliate", "rate",
                                   "credit", "debit", "when", "do",
                                   "close", "with", "hold", "use", "try",
                                   "rule", "cr", "dr", "default"]
               , P.reservedOpNames  = []
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
whiteSpace  = P.whiteSpace lexer
naturalOrFloat = P.naturalOrFloat lexer

attributeName :: Monad m => ParsecT Text st m String
attributeName = do
  c <- letter
  cs <- many $ alphaNum <|> oneOf "-."
  spaces
  return (c:cs)

pRegexp :: Monad m => ParsecT Text st m String
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

pAttributeValue :: Monad m => ParsecT Text st m AttributeValue
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

pAttributes :: Monad m => ParsecT Text st m Attributes
pAttributes = M.fromList <$> try pAttribute `sepEndBy` semicolon

pAttribute :: Monad m => ParsecT Text st m (String, AttributeValue)
pAttribute = do
  name <- attributeName
  reservedOp "="
  value <- pAttributeValue
  return (name, value)

pMessageFormat :: Monad m => ParsecT Text st m MessageFormat
pMessageFormat = many1 $ try var <|> fixed
  where
    var = do
      char '#'
      name <- many1 alphaNum
      return (MVariable name)

    fixed = MFixed <$> (many1 $ noneOf "\n\r#")

pPath :: Monad m => ParsecT Text st m Path
pPath = try identifier `sepBy1` reservedOp "/"

number :: Monad m => (st -> Char) -> (st -> Char) -> ParsecT Text st m Decimal
number getThousandsSep getDecimalSep = do
    st <- getState
    let thousandsSep = getThousandsSep st
        decimalSep   = getDecimalSep   st
    str <- many1 $ oneOf $ "0123456789" ++ [thousandsSep, decimalSep]
    let clr = filter (/= thousandsSep) str
    case length $ filter (== decimalSep) clr of
      0 -> return $ Decimal 0 (read clr)
      1 -> return $ read $ map (dot decimalSep) clr
      _ -> fail $ "More than one decimal separator in number: " ++ str
  where
    dot sep c
      | sep == c  = '.'
      | otherwise = c

currencySymbol :: Monad m => ParsecT Text st m String
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

