{-# LANGUAGE CPP, FlexibleContexts #-}

module YaLedger.Logger.STM where

import Control.Monad.State
import Control.Monad.Exception
import Control.Concurrent.STM
import System.Log.Logger (Priority (..))

import YaLedger.Types.Monad
import YaLedger.Kernel.STM (stm)
import YaLedger.Exceptions

-- | Write log message from 'Atomic' monad.
-- This puts message to 'lsMessages' queue.
logSTM :: Throws InternalError l => String -> Priority -> String -> Atomic l ()
logSTM name priority message = do
  chan <- gets lsMessages
  stm $ writeTChan chan (name, priority, message)

debugSTM :: Throws InternalError l => String -> String -> Atomic l ()
#ifdef DEBUG
debugSTM name message = logSTM INFO $ "DEBUG: " ++ name ++ ": " ++ message
#else
debugSTM _ _ = return ()
#endif

-- | Similar to 'info', but in 'Atomic' monad.
infoSTM :: Throws InternalError l => String -> String -> Atomic l ()
infoSTM name message = do
  pos <- gets lsPosition
  logSTM name INFO $ "INFO: " ++ name ++ ": " ++ message

-- | Similar to 'infoP', but in 'Atomic' monad.
infoSTMP :: Throws InternalError l => String -> String -> Atomic l ()
infoSTMP name message = do
  pos <- gets lsPosition
  logSTM name INFO $ "INFO: " ++ name ++ ": " ++ message ++ "\n    at " ++ show pos

-- | Similar to 'warning', but in 'Atomic' monad.
warningSTM :: Throws InternalError l => String -> String -> Atomic l ()
warningSTM name message = do
  pos <- gets lsPosition
  logSTM name WARNING $ "WARNING: " ++ name ++ ": " ++ message

-- | Similar to 'warningP', but in 'Atomic' monad.
warningSTMP :: Throws InternalError l => String -> String -> Atomic l ()
warningSTMP name message = do
  pos <- gets lsPosition
  logSTM name WARNING $ "WARNING: " ++ name ++ ": " ++ message ++ "\n    at " ++ show pos

