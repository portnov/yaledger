
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
  coa <- readCoA "test.accounts"
  records <- loadCSV config coa path
  forM_ records $ \rec ->
    putStrLn $ prettyPrint rec


