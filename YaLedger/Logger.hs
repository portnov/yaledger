{-# LANGUAGE CPP, FlexibleContexts #-}

module YaLedger.Logger
  (Priority (..),
   setupLogger,
   debug,
   info, warning, errorMessage,
   debugP, infoP, warningP,
   debugIO, infoIO,
   handler,
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

setupLogger :: Priority -> IO ()
setupLogger p = do
  updateGlobalLogger rootLoggerName (setLevel p)

debugP :: Throws InternalError l => String -> Ledger l ()
debugP str = do
  pos <- gets lsPosition
  debug $ str ++ "\n    at " ++ show pos

debug :: Throws InternalError l => String -> Ledger l ()
debug str =
#ifdef DEBUG
  wrapIO $ debugM rootLoggerName $ "DEBUG: " ++ str
#else
  return ()
#endif

debugIO :: String -> IO ()
debugIO str =
#ifdef DEBUG
  debugM rootLoggerName $ "DEBUG: " ++ str
#else
  return ()
#endif

errorMessage :: Throws InternalError l => String -> Ledger l ()
errorMessage str =
  wrapIO $ errorM rootLoggerName $ "ERROR: " ++ str

infoP :: Throws InternalError l => String -> Ledger l ()
infoP str = do
  pos <- gets lsPosition
  info $ str ++ "\n    at " ++ show pos

info :: Throws InternalError l => String -> Ledger l ()
info str =
  wrapIO $ infoM rootLoggerName $ "INFO: " ++ str

infoIO :: String -> IO ()
infoIO str =
  infoM rootLoggerName $ "INFO: " ++ str

warningIO :: String -> IO ()
warningIO str =
  warningM rootLoggerName $ "WARNING: " ++ str

warning :: Throws InternalError l => String -> Ledger l ()
warning str =
  wrapIO $ warningM rootLoggerName $ "WARNING: " ++ str

warningP :: Throws InternalError l => String -> Ledger l ()
warningP str = do
  pos <- gets lsPosition
  warning $ str ++ "\n    at " ++ show pos

handler loc e =
  wrapIO (emergencyM rootLoggerName $ showExceptionWithTrace loc e)

trace :: String -> a -> a
#ifdef DEBUG
trace str x = unsafePerformIO $ do
  debugM rootLoggerName $ "TRACE: " ++ str
  return x
{-# NOINLINE trace #-}
#else
trace _ x = x
#endif

traceS :: Show b => String -> b -> b
#ifdef DEBUG
traceS prefix x = unsafePerformIO $ do
  debugM rootLoggerName $ "TRACE: " ++ prefix ++ ": " ++ show x
  return x
{-# NOINLINE traceS #-}
#else
traceS _ x = x
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

