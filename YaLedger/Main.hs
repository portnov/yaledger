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
import System.Environment.XDG.BaseDir
import System.FilePath
import System.FilePath.Glob
import System.Console.GetOpt
import Text.Parsec hiding (try)

import YaLedger.Types
import YaLedger.Exceptions
import YaLedger.Types.Reports
import YaLedger.Monad
import YaLedger.Parser
import YaLedger.Parser.CSV
import YaLedger.Parser.Common (pAttribute)
import YaLedger.Kernel
import YaLedger.Processor
import YaLedger.Config
import YaLedger.Reports.Balance

parsePair :: String -> Either ParseError (String, AttributeValue)
parsePair str = runParser pAttribute () str str

parseParserConfig :: String -> Maybe (String, FilePath)
parseParserConfig str =
  case span (/= '=') str of
    (key, '=':value) -> Just (key, value)
    _ -> Nothing

parseCmdLine :: IO Options
parseCmdLine = do
  argv <- getArgs
  now <-  getCurrentDateTime
  defaultOptions <- loadConfig
  let coaF file opts =
          opts {chartOfAccounts = Just file}

      mapF file opts =
          opts {accountMap = Just file}

      fileF file opts =
          opts {files = file: files opts}

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

      pConfigF str opts =
          case parseParserConfig str of
            Just (name,value) -> opts {
                                   parserConfigs = (name,value):parserConfigs opts
                                 }
            Nothing -> error $ "Invalid parser config file specification: "
                               ++ str
                               ++ " (required: PARSER=CONFIGFILE)."
      helpF _ = Help
  let header = "Usage: yaledger [OPTIONS] [REPORT] [REPORT PARAMS]"
  let options = [
       Option "P" ["coa"] (ReqArg coaF "FILE") "Chart of accounts file to use",
       Option "M" ["map"]  (ReqArg mapF  "FILE") "Accounts map file to use",
       Option "f" ["file"] (ReqArg fileF "FILE(s)") "Input file[s]",
       Option "s" ["start"] (ReqArg startF "DATE") "Process only transactions after this date",
       Option "e" ["end"]  (ReqArg endF "DATE") "Process only transactions before this date",
       Option "a" ["attribute"]
                           (ReqArg attrF "NAME=VALUE") "Process only transactions with this attribute",
       Option "c" ["parser-config"]
                           (ReqArg pConfigF "PARSER=CONFIGFILE") "Use specified config file for this parser",
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
  dataDir <- getUserDataDir "yaledger"
  options <- parseCmdLine
  case options of
    Help -> return ()
    _ -> do
         let report = head $ reportParams options
             params = tail $ reportParams options
             inputPaths = if null (files options)
                            then [dataDir </> "default.yaledger"]
                            else files options
         case lookupInit report list of
           [] -> putStrLn $ "No such report: " ++ report ++
                            "\nSupported reports are: " ++
                            unwords (map fst list)
           [fn] -> run (chartOfAccounts options)
                       (accountMap options)
                       (parserConfigs options)
                       (query options)
                       (files options) fn params
           _ -> putStrLn $ "Ambigous report specification: " ++ report ++
                           "\nSupported reports are: " ++
                           unwords (map fst list)

tryE action =
  (Right <$> action) `catchWithSrcLoc` (\l e -> return (Left (l, e)))

run (Just coaPath) (Just mapPath) configs qry inputPaths (Report report) params = do
  coa <- readCoA coaPath
  amap <- readAMap coa mapPath
  records <- parseInputFiles configs coa inputPaths
  runLedger coa amap records $ runEMT $ do
      t <- tryE $ processRecords (filter (checkQuery qry) records)
      case t of
        Left (l, e :: SomeException) -> wrapIO $ putStrLn $ showExceptionWithTrace l e
        Right _ -> do
              x <- runGenerator (report qry) params
                     `catchWithSrcLoc`
                       (\loc (e :: InvalidCmdLine) -> do
                             wrapIO (putStrLn $ showExceptionWithTrace loc e)
                             return (return ()))
              x
run _ _ _ _ _ _ _ = error "Impossible: no coa or map file."

