{-# LANGUAGE OverlappingInstances, ScopedTypeVariables #-}
module YaLedger.Main
  (module YaLedger.Types,
   module YaLedger.Types.Reports,
   module YaLedger.Monad,
   Options (..),
   defaultMain
  ) where

import Control.Applicative ((<$>))
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Data.Maybe
import qualified Data.Map as M
import Data.List
import Data.Generics
import Data.Dates
import System.Environment
import System.FilePath
import System.FilePath.Glob
import System.Console.GetOpt
import Text.Parsec hiding (try)

import YaLedger.Types
import YaLedger.Exceptions
import YaLedger.Types.Reports
import YaLedger.Monad
import YaLedger.Parser
import YaLedger.Parser.Common (pAttribute)
import YaLedger.Kernel
import YaLedger.Processor
import YaLedger.Reports.Balance

data Options =
    Options {
      accountPlan :: FilePath,
      accountMap :: FilePath,
      files :: FilePath,
      query :: Query,
      reportParams :: [String] }
  | Help
  deriving (Eq, Show)

parsePair :: String -> Either ParseError (String, AttributeValue)
parsePair str = runParser pAttribute () str str

parseCmdLine :: IO Options
parseCmdLine = do
  argv <- getArgs
  home <- getEnv "HOME"
  now <-  getCurrentDateTime
  let configDir = home </> ".config" </> "yaledger"
      defaultOptions = Options {
        accountPlan = configDir </> "default.accounts",
        accountMap  = configDir </> "default.map",
        files = home </> ".yaledger",
        query = Query {
                  qStart = Nothing,
                  qEnd   = Just now,
                  qAttributes = M.empty },
        reportParams = ["balance"] }
      planF file opts =
          opts {accountPlan = file}
      mapF file opts =
          opts {accountMap = file}
      fileF file opts =
          opts {files = file}
      startF s opts =
          case parseDate now s of
            Right date -> opts {query = (query opts) {qStart = Just date}}
            Left err -> error $ show err
      endF s opts =
          case parseDate now s of
            Right date -> opts {query = (query opts) {qEnd = Just date}}
            Left err -> error $ show err
      attrF v opts =
          case parsePair v of
            Right (name,value) ->
              opts {query = (query opts) {
                  qAttributes = M.insert name value (qAttributes (query opts))
                  }
                }
            Left err -> error $ show err
      helpF _ = Help
  let header = "Usage: yaledger [OPTIONS] [REPORT] [REPORT PARAMS]"
  let options = [
       Option "P" ["plan"] (ReqArg planF "FILE") "Accounts plan file to use",
       Option "M" ["map"]  (ReqArg mapF  "FILE") "Accounts map file to use",
       Option "f" ["file"] (ReqArg fileF "FILE(s)") "Input file[s]",
       Option "s" ["start"] (ReqArg startF "DATE") "Process only transactions after this date",
       Option "e" ["end"]  (ReqArg endF "DATE") "Process only transactions before this date",
       Option "a" ["attribute"] (ReqArg attrF "NAME=VALUE") "Process only transactions with this attribute",
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

lookupInit :: String -> [(String, a)] -> [a]
lookupInit key list = [v | (k,v) <- list, key `isPrefixOf` k]

defaultMain :: [(String, Report)] -> IO ()
defaultMain list = do
  options <- parseCmdLine
  case options of
    Help -> return ()
    _ -> do
         let report = head $ reportParams options
             params = tail $ reportParams options
         case lookupInit report list of
           [] -> putStrLn $ "No such report: " ++ report ++
                            "\nSupported reports are: " ++
                            unwords (map fst list)
           [fn] -> run (accountPlan options)
                       (accountMap options)
                       (query options)
                       (files options) fn params
           _ -> putStrLn $ "Ambigous report specification: " ++ report ++
                           "\nSupported reports are: " ++
                           unwords (map fst list)

try action =
  (Right <$> action) `catchWithSrcLoc` (\l e -> return (Left (l, e)))

run planPath mapPath qry inputPath (Report report) params = do
  plan <- readPlan planPath
  amap <- readAMap plan mapPath
  records <- readTrans plan inputPath
  runLedger plan amap $ runEMT $ do
      t <- try $ processRecords (filter (checkQuery qry) records)
      case t of
        Left (l, e :: SomeException) -> wrapIO $ putStrLn $ showExceptionWithTrace l e
        Right _ -> do
              x <- runGenerator (report qry) params
                     `catchWithSrcLoc`
                       (\loc (e :: InvalidCmdLine) -> do
                             wrapIO (putStrLn $ showExceptionWithTrace loc e)
                             return (return ()))
              x

