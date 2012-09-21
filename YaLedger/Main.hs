{-# LANGUAGE DeriveDataTypeable, OverlappingInstances, ScopedTypeVariables #-}
module YaLedger.Main
  (module YaLedger.Types,
   module YaLedger.Types.Reports,
   module YaLedger.Monad,
   Options (..),
   defaultMain
  ) where

import Control.Monad.Exception
import Control.Monad.Exception.Base
import Data.Maybe
import Data.Generics
import System.Environment
import System.FilePath
import System.FilePath.Glob
import System.Console.GetOpt

import YaLedger.Types
import YaLedger.Exceptions
import YaLedger.Types.Reports
import YaLedger.Monad
import YaLedger.Parser
import YaLedger.Processor
import YaLedger.Reports.Balance

data Options =
    Options {
      accountPlan :: FilePath,
      accountMap :: FilePath,
      files :: FilePath,
      reportParams :: [String] }
  | Help
  deriving (Eq, Show, Data, Typeable)

parseCmdLine :: IO Options
parseCmdLine = do
  argv <- getArgs
  home <- getEnv "HOME"
  let configDir = home </> ".config" </> "yaledger"
      defaultOptions = Options {
        accountPlan = configDir </> "default.accounts",
        accountMap  = configDir </> "default.map",
        files = home </> ".yaledger",
        reportParams = ["balance"] }
      planF file opts =
          opts {accountPlan = file}
      mapF file opts =
          opts {accountMap = file}
      fileF file opts =
          opts {files = file}
      helpF _ = Help
  let header = "Usage: yaledger [OPTIONS] [REPORT] [REPORT PARAMS]"
  let options = [
       Option "P" ["plan"] (ReqArg planF "FILE") "Accounts plan file to use",
       Option "M" ["map"]  (ReqArg mapF  "FILE") "Accounts map file to use",
       Option "f" ["file"] (ReqArg fileF "FILE(s)") "Input file[s]",
       Option "h" ["help"] (NoArg helpF) "Show this help and exit" ]
  case getOpt RequireOrder options argv of
    (fns, params, []) -> do
        let res = foldl (flip id) defaultOptions fns
        case res of
          Help -> do
                  putStrLn $ usageInfo header options
                  return Help
          _ -> if null params
                 then return res
                 else return $ res { reportParams = params }
    (_,_,errs) -> fail $ concat errs ++ usageInfo header options

defaultMain :: [(String, Report)] -> IO ()
defaultMain list = do
  options <- parseCmdLine
  print options
  case options of
    Help -> return ()
    _ -> do
         let report = head $ reportParams options
             params = tail $ reportParams options
         case lookup report list of
           Nothing -> fail $ "No such report: " ++ report
           Just fn -> run (accountPlan options) (accountMap options) (files options) fn params

run planPath mapPath inputPath (Report report) params = do
  plan <- readPlan planPath
  amap <- readAMap plan mapPath
  records <- readTrans plan inputPath
  runLedger plan amap $ runEMT $ do
      processRecords records
        `catchWithSrcLoc`
          (handler :: EHandler SomeException)
      x <- runGenerator report params
             `catchWithSrcLoc`
               (\loc (e :: InvalidCmdLine) -> do
                     wrapIO (putStrLn $ showExceptionWithTrace loc e)
                     return (return ()))
      x

