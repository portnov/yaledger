{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, DeriveDataTypeable, MultiParamTypeClasses, UndecidableInstances #-}

module YaLedger.Exceptions.Utils
  (wrapE)
  where

import Control.Monad.Exception

import YaLedger.Types.Common
import YaLedger.Exceptions

wrapE :: (Monad m, Throws InternalError l)
      => EMT (Caught SomeException (Caught FailException l)) m a
      -> EMT l m a
wrapE action = wrapException wrapFail $ wrapException wrapSome action
  where
    wrapFail (FailException msg) = InternalError msg nowhere
    wrapSome (SomeException e)   = InternalError (show e) nowhere

