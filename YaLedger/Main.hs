{-# LANGUAGE OverlappingInstances, ScopedTypeVariables #-}
module YaLedger.Main
  (module YaLedger.Types,
   module YaLedger.Types.Reports,
   module YaLedger.Monad,
   LedgerOptions (..),
   parseCmdLine,
   defaultMain,
   lookupInit,
   runYaLedger
  ) where

import Prelude hiding (catch)
import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import Data.List
import Data.Dates
import System.Environment
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
import YaLedger.Config
import YaLedger.Logger
import YaLedger.Pretty
import YaLedger.Reports.Common

parsePair :: String -> Either ParseError (String, AttributeValue)
parsePair str = runParser pAttribute () str str

parseParserConfig :: String -> Maybe (String, FilePath)
parseParserConfig str =
  case span (/= '=') str of
    (key, '=':value) -> Just (key, value)
    _ -> Nothing

parseDebug :: String -> Maybe Priority
parseDebug str =
  case reads (map toUpper str) of
    [(x, "")] -> Just x
    _ -> Nothing

parseCmdLine :: [String] -> IO LedgerOptions
parseCmdLine argv = do
  now <-  getCurrentDateTime
  defaultOptions <- loadConfig
  let coaF file opts =
          opts {chartOfAccounts = Just file}

      mapF file opts =
          opts {accountMap = Just file}

      fileF Nothing opts = opts {files = []}
      fileF (Just file) opts =
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

      allAdmin opts =
         opts {query = (query opts) {qAllAdmin = True}}

      intervalF str opts =
          case runParser pDateInterval () str str of
            Left err -> error $ show err
            Right int -> opts {reportsInterval = Just int}

      debugF str opts =
          case parseDebug str of
            Just value -> opts {logSeverity = value}
            Nothing -> error $ "Unknown debug level: " ++ str

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
       Option "C" ["coa"] (ReqArg coaF "FILE") "Chart of accounts file to use",
       Option "M" ["map"]  (ReqArg mapF  "FILE") "Accounts map file to use",
       Option "f" ["file"] (OptArg fileF "FILE(s)") "Input file[s]",
       Option "s" ["start"] (ReqArg startF "DATE") "Process only transactions after this date",
       Option "e" ["end"]  (ReqArg endF "DATE") "Process only transactions before this date",
       Option "A" ["all-admin"] (NoArg allAdmin) "Process all admin records with any dates and attributes",
       Option "a" ["attribute"]
                           (ReqArg attrF "NAME=VALUE") "Process only transactions with this attribute",
       Option "P" ["period"] (ReqArg intervalF "PERIOD") "Output report by PERIOD",
       Option ""  ["daily"] (NoArg (\opts -> opts {reportsInterval = Just (Days 1)}))
                              "Alias for --period \"1 day\"",
       Option ""  ["weekly"] (NoArg (\opts -> opts {reportsInterval = Just (Weeks 1)}))
                              "Alias for --period \"1 week\"",
       Option ""  ["monthly"] (NoArg (\opts -> opts {reportsInterval = Just (Months 1)}))
                              "Alias for --period \"1 month\"",
       Option ""  ["yearly"] (NoArg (\opts -> opts {reportsInterval = Just (Years 1)}))
                              "Alias for --period \"1 year\"",
       Option "d" ["debug"] (ReqArg debugF "LEVEL") "Set debug level to LEVEL",
       Option "p" ["parser-config"]
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
  argv <- getArgs
  options <- parseCmdLine argv
  case options of
    Help -> putStrLn $ "Supported reports are: " ++ unwords (map fst list)
    _ -> do
         let report = head $ reportParams options
             params = tail $ reportParams options
         case lookupInit report list of
           [] -> putStrLn $ "No such report: " ++ report ++
                            "\nSupported reports are: " ++
                            unwords (map fst list)
           [fn] -> do
               setupLogger (logSeverity options)
               runYaLedger (chartOfAccounts options)
                   (accountMap options)
                   (parserConfigs options)
                   (reportsInterval options)
                   (query options)
                   (deduplicationRules options)
                   (files options) fn params
           _ -> putStrLn $ "Ambigous report specification: " ++ report ++
                           "\nSupported reports are: " ++
                           unwords (map fst list)

tryE action =
  (Right <$> action) `catchWithSrcLoc` (\l e -> return (Left (l, e)))

showInterval :: Query -> String
showInterval qry =
    "From " ++ showMD "the begining" (qStart qry) ++ " till " ++ showMD "now" (qEnd qry)
  where
    showMD s Nothing = s
    showMD _ (Just date) = prettyPrint date

runYaLedger (Just coaPath) (Just mapPath) configs mbInterval qry rules inputPaths report params = do
  coa <- readCoA coaPath
  amap <- readAMap coa mapPath
  records <- parseInputFiles configs coa inputPaths
  runLedger coa amap records $ runEMT $
    processYaLedger qry mbInterval rules (filter (checkRecord qry) records) report params
runYaLedger _ _ _ _ _ _ _ _ _ = error "Impossible: no coa or map file."

processYaLedger qry mbInterval rules records report params = do
  now <- gets lsStartDate
  let endDate = fromMaybe now (qEnd qry)
  t <- tryE $ processRecords endDate rules records 
  case t of
    Left (l, e :: SomeException) -> wrapIO $ putStrLn $ showExceptionWithTrace l e
    Right _ -> do
      now <- wrapIO getCurrentDateTime
      let firstDate = minimum (map getDate records)
      let queries = case mbInterval of
                      Nothing -> [qry]
                      Just int -> splitQuery firstDate now qry int
      forM_ queries $ \query -> do
          wrapIO $ putStrLn $ showInterval query
          runAReport query params report
             `catch`
               (\(e :: InvalidCmdLine) -> do
                     wrapIO (putStrLn $ show e))

