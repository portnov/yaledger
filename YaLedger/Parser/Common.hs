module YaLedger.Parser.Common where

import Control.Applicative
import Data.Maybe
import Data.List
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Text.Printf

import YaLedger.Types
import YaLedger.Tree

import Debug.Trace

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

pPath :: Parsec String st Path
pPath = identifier `sepBy` reservedOp "/"

getAccount :: (st -> AccountPlan) -> Path -> Parsec String st AnyAccount
getAccount accountPlan path = do
  st <- getState
  case lookupTree path (accountPlan st) of
    [] -> fail $ "No such account: " ++ intercalate "/" path
    [a] -> return a
    as -> fail $ printf "Ambigous account specification: %s (%d matching accounts)."
                        (intercalate "/" path)
                        (length as)

