{-# LANGUAGE CPP, MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}

module YaLedger.Kernel.STM where

import Control.Monad.Exception
import Control.Monad.State
import Control.Concurrent.STM

import YaLedger.Exceptions
import YaLedger.Exceptions.Utils
import YaLedger.Types.Monad

-- | Lift STM action into 'Atomic' monad.
stm :: Throws InternalError l => STM a -> Atomic l a
stm action = EMT $ LedgerStateT $ StateT $ \ls -> do
               result <- action
               return (Right result, ls)

-- | Wrap IO action into EMT monad
wrapIO :: (MonadIO m, Throws InternalError l)
       => IO a
       -> EMT l m a
wrapIO action = wrapE $ liftIO action

