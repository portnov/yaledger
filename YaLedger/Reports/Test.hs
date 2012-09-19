{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeOperators, ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Reports.Test where

import Control.Monad.Exception
import Control.Monad.Loc
import Data.Dates

import YaLedger.Types
import YaLedger.Exceptions
import YaLedger.Monad
import YaLedger.Parser
import YaLedger.Reports.Common

test :: Report String -> [String] -> IO ()
test (Report gen) list = do
  plan <- readPlan "test.accounts"
  amap <- readAMap plan "test.map"
  runLedger plan amap $ runEMT $ do
      str <- runGenerator gen list
      wrapIO $ putStrLn str
    `catchWithSrcLoc`
      (\loc (e :: InvalidCmdLine) -> wrapIO $ putStrLn (showExceptionWithTrace loc e))

testF :: Path -> DateTime -> String
testF path dt = show path ++ ", " ++ show dt

