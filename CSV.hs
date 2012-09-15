
module Main where

import Control.Monad
import System.Environment

import YaLedger.Types
import YaLedger.Tree
import YaLedger.Pretty
import YaLedger.Parser
import YaLedger.Parser.CSV

main = do
  [config, path] <- getArgs
  plan <- readPlan "test.accounts"
  records <- loadCSV plan config path
  forM_ records $ \rec ->
    putStrLn $ prettyPrint rec


