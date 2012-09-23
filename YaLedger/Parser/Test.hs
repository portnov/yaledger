
module YaLedger.Parser.Test where

import Text.Parsec

import YaLedger.Types
import YaLedger.Parser
import qualified YaLedger.Parser.Common as C
import qualified YaLedger.Parser.Transactions as T
import qualified YaLedger.Parser.Map as M
import qualified YaLedger.Parser.Plan as P
import qualified YaLedger.Parser.CSV as CSV

runTParser :: T.Parser a -> String -> IO a
runTParser p str = do
  plan <- readPlan "test.accounts" 
  st <- T.emptyPState plan
  case runParser p st "<>" str of
    Left err -> fail $ show err
    Right x -> return x
