
module Test where

import Text.Parsec

import Types
import Tree
import Kernel
import Parser

test :: IO AccountPlan
test = do
  let fileName = "test.accounts"
  content <- readFile fileName
  case runParser pAccountGroup emptyPState fileName content of
    Right res -> return res
    Left err -> fail $ show err
