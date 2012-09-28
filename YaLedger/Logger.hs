{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}

module YaLedger.Logger
  (Priority (..),
   setupLogger,
   debug,
   info, warning, errorMessage,
   handler
  ) where

import Control.Monad.Exception
import Control.Monad.Loc
import System.Log.Logger
import System.Log.Handler.Simple
import System.IO

import YaLedger.Monad
import YaLedger.Exceptions

setupLogger :: Priority -> IO ()
setupLogger p = do
  h <- streamHandler stderr  p
  updateGlobalLogger rootLoggerName (addHandler h)
  updateGlobalLogger rootLoggerName (setLevel p)

debug :: Throws InternalError l => String -> Ledger l ()
debug str =
  wrapIO $ debugM rootLoggerName $ ">> " ++ str

errorMessage :: Throws InternalError l => String -> Ledger l ()
errorMessage str =
  wrapIO $ errorM rootLoggerName $ ">> " ++ str

info :: Throws InternalError l => String -> Ledger l ()
info str =
  wrapIO $ infoM rootLoggerName $ ">> " ++ str

warning :: Throws InternalError l => String -> Ledger l ()
warning str =
  wrapIO $ warningM rootLoggerName $ ">> " ++ str

handler loc e =
  wrapIO (emergencyM rootLoggerName $ showExceptionWithTrace loc e)

