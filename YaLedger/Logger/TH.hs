{-# LANGUAGE TemplateHaskell #-}

module YaLedger.Logger.TH
  (debug, info, warning, errorMessage,
   debugP, infoP, warningP,
   debugIO, infoIO,
   debugSTM, infoSTM, warningSTM,
   infoSTMP, warningSTMP,
   trace, traceS,
   allLoggers
  ) where

import Control.Monad
import Language.Haskell.TH
import System.Directory
import System.FilePath.Glob

import qualified YaLedger.Logger.Loggers as L
import qualified YaLedger.Logger.STM as STM

listModules :: IO [String]
listModules = do
    pwd <- getCurrentDirectory
    paths <- globDir1 (compile "**/*.hs") pwd
    let result = map (clear $ length pwd + 1) paths
    forM_ result putStrLn
    return result
  where
    clear n path = replaceSlash $ drop n $ take (length path - 3) path

    replaceSlash [] = []
    replaceSlash ('/':s) = '.': replaceSlash s
    replaceSlash (c:s) = c : replaceSlash s

allLoggers :: Q Exp
allLoggers = do
  modules <- runIO listModules
  return $ ListE $ map (LitE . StringL) modules

getLogger :: Q String
getLogger = do
  loc <- location
  return $ loc_module loc

string :: String -> Q Exp
string = return . LitE . StringL

liftQ :: Name -> Q Exp
liftQ name = do
  logger <- getLogger
  [| $(varE name) $(string logger) |]

liftQ2 :: Name -> Q Exp
liftQ2 name = do
  logger <- getLogger
  [| \str x -> $(varE name) $(string logger) str x |]

debug :: Q Exp
debug = liftQ 'L.debug

debugIO :: Q Exp
debugIO = liftQ 'L.debugIO

debugP :: Q Exp
debugP = liftQ 'L.debugP

debugSTM :: Q Exp
debugSTM = liftQ 'STM.debugSTM

info :: Q Exp
info = liftQ 'L.info

infoP :: Q Exp
infoP = liftQ 'L.infoP

infoIO :: Q Exp
infoIO = liftQ 'L.infoIO

infoSTM :: Q Exp
infoSTM = liftQ 'STM.infoSTM

infoSTMP :: Q Exp
infoSTMP = liftQ 'STM.infoSTMP

warning :: Q Exp
warning = liftQ 'L.warning

warningP :: Q Exp
warningP = liftQ 'L.warningP

warningIO :: Q Exp
warningIO = liftQ 'L.warningIO

warningSTM :: Q Exp
warningSTM = liftQ 'STM.warningSTM

warningSTMP :: Q Exp
warningSTMP = liftQ 'STM.warningSTMP

errorMessage :: Q Exp
errorMessage = liftQ 'L.errorMessage

trace :: Q Exp
trace = liftQ2 'L.trace

traceS :: Q Exp
traceS = liftQ2 'L.traceS

