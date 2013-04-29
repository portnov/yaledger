{-# LANGUAGE CPP, FlexibleContexts #-}

module YaLedger.Logger.Loggers
  (debug, info, warning, errorMessage,
   debugP, infoP, warningP,
   debugIO, infoIO, warningIO,
   trace, traceS,
   traceEventIO, traceEventM
  ) where


import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import System.Log.Logger

#ifdef DEBUG
import System.IO.Unsafe
import qualified Debug.Trace as Trace
#endif

import YaLedger.Types
import YaLedger.Exceptions
import YaLedger.Types.Monad.STM

debugP :: Throws InternalError l => String -> String -> Ledger l ()
debugP name str = do
  pos <- gets lsPosition
  debug name $ str ++ "\n    at " ++ show pos

debug :: Throws InternalError l => String -> String -> Ledger l ()
debug name str =
#ifdef DEBUG
  wrapIO $ debugM name $ "DEBUG: " ++ name ++ ": " ++ str
#else
  return ()
#endif

debugIO :: String -> String -> IO ()
debugIO name str =
#ifdef DEBUG
  debugM name $ "DEBUG: " ++ name ++ ": " ++ str
#else
  return ()
#endif

errorMessage :: Throws InternalError l => String -> String -> Ledger l ()
errorMessage name str =
  wrapIO $ errorM name $ "ERROR: " ++ name ++ ": " ++ str

infoP :: Throws InternalError l => String -> String -> Ledger l ()
infoP name str = do
  pos <- gets lsPosition
  info name $ str ++ "\n    at " ++ show pos

info :: Throws InternalError l => String -> String -> Ledger l ()
info name str =
  wrapIO $ infoM name $ "INFO: " ++ name ++ ": " ++ str

infoIO :: String -> String -> IO ()
infoIO name str =
  infoM name $ "INFO: " ++ name ++ ": " ++ str

warningIO :: String -> String -> IO ()
warningIO name str =
  warningM name $ "WARNING: " ++ name ++ ": " ++ str

warning :: Throws InternalError l => String -> String -> Ledger l ()
warning name str =
  wrapIO $ warningM name $ "WARNING: " ++ name ++ ": " ++ str

warningP :: Throws InternalError l => String -> String -> Ledger l ()
warningP name str = do
  pos <- gets lsPosition
  warning name $ str ++ "\n    at " ++ show pos

trace :: String -> String -> a -> a
#ifdef DEBUG
trace name str x = unsafePerformIO $ do
  debugM name $ "TRACE: " ++ name ++ ": " ++ str
  return x
{-# NOINLINE trace #-}
#else
trace _ _ x = x
#endif

traceS :: Show b => String -> String -> b -> b
#ifdef DEBUG
traceS name prefix x = unsafePerformIO $ do
  debugM name $ "TRACE: " ++ name ++ ": " ++ prefix ++ ": " ++ show x
  return x
{-# NOINLINE traceS #-}
#else
traceS _ _ x = x
#endif

traceEventM :: Throws InternalError l => String -> Ledger l ()
#ifdef DEBUG
traceEventM message = wrapIO (Trace.traceEventIO message)
{-# NOINLINE traceEventIO #-}
#else
traceEventM _ = return ()
#endif

traceEventIO :: String -> IO ()
#ifdef DEBUG
traceEventIO message = Trace.traceEventIO message
{-# NOINLINE traceEventM #-}
#else
traceEventIO _ = return ()
#endif

