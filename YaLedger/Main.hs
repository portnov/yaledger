{-# LANGUAGE OverlappingInstances, ScopedTypeVariables, TemplateHaskell #-}
module YaLedger.Main
  (module YaLedger.Types,
   module YaLedger.Types.Reports,
   LedgerOptions (..),
   parseCmdLine,
   allParsers,
   defaultMain,
   lookupInit,
   initialize,
   runYaLedger
  ) where

import Prelude hiding (catch)
import Control.Applicative ((<$>))
import qualified Control.Exception as E
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Control.Concurrent.ParallelIO
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import Data.List
import Data.Monoid
import Data.Dates
import System.Environment
import System.Console.GetOpt
import Text.Parsec hiding (try)
import System.FilePath
import System.Environment.XDG.BaseDir
import System.Log.Logger

import YaLedger.Types
import YaLedger.Exceptions
import YaLedger.Types.Reports
import YaLedger.Parser
import YaLedger.Parser.Common (pAttribute)
import YaLedger.Parser.Currencies
import YaLedger.Kernel
import YaLedger.Kernel.Correspondence (buildGroupsMap)
import YaLedger.Processor
import YaLedger.Config
import YaLedger.Logger
import YaLedger.Reports.Common
import YaLedger.Installer

-- | Parrse NAME=VALUE attribute syntax
parsePair :: String -> Either ParseError (String, AttributeValue)
parsePair str = runParser pAttribute () str str

-- | Parse PARSER=CONFIG syntax
parseParserConfig :: String -> Maybe (String, FilePath)
parseParserConfig str =
  case span (/= '=') str of
    (key, '=':value) -> Just (key, value)
    _ -> Nothing

-- | Parse debug level (debug, warning etc)
parseDebug :: String -> Maybe Priority
parseDebug str =
  case reads (map toUpper str) of
    [(x, "")] -> Just x
    _ -> Nothing

parseDebugPair :: String -> Maybe (String, Priority)
parseDebugPair str =
  case break (== '=') str of
    (s,"") -> (\p -> ("", p)) <$> parseDebug s
    (name, _:s) -> (\p -> (name, p)) <$> parseDebug s

-- | Apply SetOption to LedgerOptions
apply :: SetOption -> LedgerOptions -> LedgerOptions
apply (SetConfigPath path) opts = opts {mainConfigPath = Just path}
apply (SetCoAPath path)    opts = opts {chartOfAccounts = Just path}
apply (SetAMapPath path)   opts = opts {accountMap = Just path}
apply (SetCurrenciesPath path) opts = opts {currenciesList = Just path}
apply (AddFile (Just path)) opts = opts {files = path: files opts}
apply (AddFile Nothing)    opts = opts {files = []}
apply (SetStartDate date)  opts = opts {query = (query opts) {qStart = Just date}}
apply (SetEndDate date)    opts = opts {query = (query opts) {qEnd = Just date}}
apply (SetAllAdmin)        opts = opts {query = (query opts) {qAllAdmin = True}}
apply (AddAttribute (n,v)) opts = let qry = query opts
                                  in  opts {query = qry {qAttributes = M.insert n v (qAttributes qry)}}
apply (SetReportStart date) opts = let qry = reportsQuery opts
                                   in  opts {reportsQuery = qry {qStart = Just date}}
apply (SetReportEnd   date) opts = let qry = reportsQuery opts
                                   in  opts {reportsQuery = qry {qEnd = Just date}}
apply (SetReportsInterval i) opts = opts {reportsInterval = Just i}
apply (SetDebugLevel ("", lvl))  opts = opts {defaultLogSeverity = lvl}
apply (SetDebugLevel (name, lvl))  opts = opts {logSeveritySetup = logSeveritySetup opts ++ [(name,lvl)] }
apply (SetParserConfig (n,p)) opts = opts {parserConfigs = (n,p): parserConfigs opts}
apply SetHelp _ = Help
                         
-- | Parse command line
parseCmdLine :: [String] -> IO LedgerOptions
parseCmdLine argv = do
  now <-  getCurrentDateTime
  let attr v =
          case parsePair v of
            Right (name,value) -> (name, value)
            Left err -> error $ show err

      interval str =
          case runParser pDateInterval () str str of
            Left err -> error $ show err
            Right int -> int

      level str =
          case parseDebugPair str of
            Just value -> value
            Nothing -> error $ "Unknown debug level: " ++ str

      date str = 
          case parseDate now str of
            Right date -> date
            Left err -> error $ show err

      pconfig str =
          case parseParserConfig str of
            Just (name,value) -> (name, value)
            Nothing -> error $ "Invalid parser config file specification: "
                               ++ str
                               ++ " (required: PARSER=CONFIGFILE)."

  let header = "Usage: yaledger [OPTIONS] [REPORT] [REPORT PARAMS]\n\
               \   or: yaledger init [DIRECTORY]\n\
               \Supported options are:"
  let options = [
       Option "c" ["config"] (ReqArg SetConfigPath "FILE")
                   "Use specified config file instead of ~/.config/yaledger/yaledger.yaml",
       Option "C" ["coa"] (ReqArg SetCoAPath "FILE")
                   "Chart of accounts file to use",
       Option "M" ["map"]  (ReqArg SetAMapPath  "FILE")
                   "Accounts map file to use",
       Option "r" ["currencies"] (ReqArg SetCurrenciesPath "FILE")
                   "Currencies list file to use",
       Option "f" ["file"] (OptArg AddFile "FILE(s)")
                   "Input file[s]",
       Option "s" ["start"] (ReqArg (SetStartDate . date) "DATE")
                   "Process only transactions after this date",
       Option "e" ["end"]  (ReqArg (SetEndDate . date) "DATE")
                   "Process only transactions before this date",
       Option "S" ["report-from"] (ReqArg (SetReportStart . date) "DATE")
                   "Start report from this date",
       Option "E" ["report-to"] (ReqArg (SetReportEnd . date) "DATE")
                   "End report at this date",
       Option "A" ["all-admin"] (NoArg SetAllAdmin)
                   "Process all admin records with any dates and attributes",
       Option "a" ["attribute"] (ReqArg (AddAttribute . attr) "NAME=VALUE")
                   "Process only transactions with this attribute",
       Option "P" ["period"] (ReqArg (SetReportsInterval . interval) "PERIOD")
                   "Output report by PERIOD",
       Option ""  ["daily"] (NoArg (SetReportsInterval $ Days 1))
                   "Alias for --period \"1 day\"",
       Option ""  ["weekly"] (NoArg (SetReportsInterval $ Weeks 1))
                   "Alias for --period \"1 week\"",
       Option ""  ["monthly"] (NoArg (SetReportsInterval $ Months 1))
                   "Alias for --period \"1 month\"",
       Option ""  ["yearly"] (NoArg (SetReportsInterval $ Years 1))
                   "Alias for --period \"1 year\"",
       Option "d" ["debug"] (ReqArg (SetDebugLevel . level) "[MODULE=]LEVEL")
                   "Set debug level (for MODULE or for all modules) to LEVEL",
       Option "p" ["parser-config"] (ReqArg (SetParserConfig . pconfig) "PARSER=CONFIGFILE")
                   "Use specified config file for this parser",
       Option "h" ["help"] (NoArg SetHelp)
                   "Show this help and exit" ]
  case getOpt RequireOrder options argv of
    (fns, params, []) -> do
      if SetHelp `elem` fns
        then do
             putStrLn $ usageInfo header options
             return Help
        else do
          configPath <- case [p | SetConfigPath p <- fns] of
                          [] -> do
                                configDir <- getUserConfigDir "yaledger"
                                return (configDir </> "yaledger.yaml")
                          (p:_) -> return p
          defaultOptions <- loadConfig configPath
          case foldr apply defaultOptions (reverse fns) of
            Help -> fail "Impossible: Main.parseCmdLine.Help"
            opts -> do
               let opts' = opts {
                             mainConfigPath = Just configPath,
                             reportsQuery = query opts `mappend` reportsQuery opts
                           }
               if null params
                 then return opts'
                 else return $ opts' { defaultReport = head params,
                                       reportParams  = tail params }
    (_,_,errs) -> fail $ concat errs ++ usageInfo header options

-- | Lookup for items with keys starting with given prefix
lookupInit :: String -> [(String, a)] -> [a]
lookupInit key list = [v | (k,v) <- list, key `isPrefixOf` k]

initialize :: [(String, String, InputParser)] -- ^ List of parsers to support: (parser name, files mask, parser)
            -> [(String, Report)]              -- ^ List of reports to support: (report name, report)
            -> [String]
            -> IO (Maybe (Report, LedgerOptions, [String]))
initialize parsers reports argv = do
  options <- parseCmdLine argv
  case options of
    Help -> do
         putStrLn $ "Supported reports are: " ++ unwords (map fst reports)
         return Nothing
    _ -> do
         setupLogger (defaultLogSeverity options) (logSeveritySetup options)
         $infoIO $ "Using chart of accounts: " ++ fromJust (chartOfAccounts options)
         $infoIO $ "Using accounts map: " ++ fromJust (accountMap options)
         $debugIO $ "Using parser configs:\n" ++
             (unlines $ map (\(name,config) -> name ++ ": " ++ config) (parserConfigs options))
         let report = defaultReport options
         params <- if null (reportParams options)
                     then case lookupInit report (M.assocs $ defaultReportParams options) of
                            [] -> return []
                            [str] -> return $ words str
                            list -> do
                                    putStrLn $ "Ambigous report specification: " ++ report ++
                                               "\nSupported reports are: " ++
                                               unwords (map fst reports)
                                    return []
                        else return $ reportParams options
         case lookupInit report reports of
           [] -> do
                 putStrLn $ "No such report: " ++ report ++
                            "\nSupported reports are: " ++
                            unwords (map fst reports)
                 return Nothing
           [fn] -> return $ Just (fn, options, params)
           _ -> do
                putStrLn $ "Ambigous report specification: " ++ report ++
                           "\nSupported reports are: " ++
                           unwords (map fst reports)
                return Nothing

-- | Default `main' function
defaultMain :: [(String, String, InputParser)] -- ^ List of parsers to support: (parser name, files mask, parser)
            -> [(String, Report)]              -- ^ List of reports to support: (report name, report)
            -> IO ()
defaultMain parsers reports = do
  argv <- getArgs
  if (not $ null argv) && (head argv == "init")
    then install $ tail argv
    else do
         init <- initialize parsers reports argv
         case init of
           Nothing -> return ()
           Just (report, options, params) ->
             runYaLedger parsers options report params

tryE action =
  (Right <$> action) `catchWithSrcLoc` (\l e -> return (Left (l, e)))

fromJustM msg Nothing = fail msg
fromJustM _ (Just x)  = return x

loadConfigs :: FilePath -> FilePath -> FilePath -> IO (Currencies, ChartOfAccounts, AccountMap)
loadConfigs cpath coaPath mapPath = do
  currs <- loadCurrencies cpath
  let currsMap = M.fromList [(cSymbol c, c) | c <- currs]
  coa <- readCoA currsMap coaPath
  amap <- readAMap coa mapPath
  return (currsMap, coa, amap)

runYaLedger :: [(String, String, InputParser)]
            -> LedgerOptions
            -> Report
            -> [String]
            -> IO ()
runYaLedger parsers options report params = do
  coaPath <- fromJustM "No CoA path specified" (chartOfAccounts options)
  mapPath <- fromJustM "No accounts map path specified" (accountMap options)
  cpath   <- fromJustM "No currencies list file specified" (currenciesList options)
  let configs = ("yaledger", fromJust (mainConfigPath options)): parserConfigs options
      mbInterval = reportsInterval options
      rules = deduplicationRules options
      inputPaths = files options
      qry = query options
      qryReport = reportsQuery options
  $debugIO $ "Deduplication rules: " ++ show rules
  (currsMap, coa, amap) <- loadConfigs cpath coaPath mapPath
                           `E.catch`
                            (\(e :: SomeException) -> do
                                fail $ "An error occured while loading configs:\n  " ++ show e ++
                                       "\nIf you do not have any configs, just run `yaledger init' command." )


  records <- parseInputFiles options parsers configs currsMap coa inputPaths
  $debugIO $ "Records loaded."
  runLedger options coa amap records $ runEMT $ do
    -- Build full map of groups of accounts.
    modify $ \st -> st {
                      lsFullGroupsMap = buildGroupsMap coa [1.. snd (agRange $ branchData coa)]
                    }
    processYaLedger qry mbInterval rules (filter (checkRecord qry) records) qryReport report params
  stopGlobalPool

processYaLedger qry mbInterval rules records qryReport report params = do
  now <- gets lsStartDate
  let endDate = fromMaybe now (qEnd qry)
  $info $ "Records query: " ++ show qry
  $info $ "Report query: " ++ show qryReport
  t <- tryE $ processRecords endDate rules records 
  case t of
    Left (l, e :: SomeException) -> wrapIO $ putStrLn $ showExceptionWithTrace l e
    Right _ -> do
      let firstDate = minimum (map getDate records)
      let queries = case mbInterval of
                      Nothing -> [qryReport]
                      Just int -> splitQuery firstDate now qryReport int

      runAReport queries params report
         `catch`
           (\(e :: InvalidCmdLine) -> do
                 wrapIO (putStrLn $ show e))

