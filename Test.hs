
module Test where

import Control.Monad
import Text.Parsec

import YaLedger.Types
import YaLedger.Tree
import YaLedger.Kernel
import qualified YaLedger.Parser.Plan as Plan
import qualified YaLedger.Parser.Transactions as T

test :: IO ()
test = do
  let planName = "test.accounts"
  content <- readFile planName
  plan <- case runParser Plan.pAccountGroup Plan.emptyPState planName content of
            Right res -> return res
            Left err -> fail $ show err
  let fileName = "test.yaledger"
  str <- readFile fileName
  st <- T.emptyPState plan
  trans <- case runParser T.pTransactions st fileName str of
             Right res -> return res
             Left err -> fail $ show err
  forM trans print
  return ()
