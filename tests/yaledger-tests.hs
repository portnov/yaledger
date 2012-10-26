
module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Data.Maybe
import Data.List

import YaLedger.Types
import YaLedger.Kernel
import YaLedger.Main
import YaLedger.Exceptions
import YaLedger.Tests.Instances
import YaLedger.Tests.Correspondence
import qualified YaLedger.Tests.Main as Test

main :: IO ()
main =
  Test.main "-c ../examples/tests.yaml"



