module YaLedger.Parser.Common where

import Control.Applicative
import Data.Maybe
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

import YaLedger.Types

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
float       = P.float lexer

pAttributes :: Parsec String st Attributes
pAttributes = try attribute `sepEndBy` semicolon
  where
    attribute = do
      name <- identifier
      reservedOp "="
      value <- stringLit
      return (name, value)

