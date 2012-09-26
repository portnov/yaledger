{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeOperators, ScopedTypeVariables, ExistentialQuantification, OverlappingInstances #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
module YaLedger.Types.Reports where

import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc
import Data.Dates

import YaLedger.Tree
import YaLedger.Exceptions
import YaLedger.Types.Ledger
import YaLedger.Types.Transactions
import YaLedger.Monad
import YaLedger.Kernel (getAccount)

class ReportGenerator a r where
  runGenerator :: (Throws InvalidCmdLine l,
                   Throws InternalError l)
               => a -> [String] -> Ledger l r

data Report =
  forall a. (ReportGenerator a (Ledger NoExceptions ()))
           => Report (Query -> a)

instance ReportGenerator r r where
  runGenerator r [] = return r
  runGenerator _ lst = throw (InvalidCmdLine $ "Too many parameters: " ++ unwords lst)

instance (ReportGenerator a r, ReportParameter p)
      => ReportGenerator (p -> a) r where
  runGenerator _ [] = throw (InvalidCmdLine "Not enough parameters")
  runGenerator f (x:xs) = do
    p <- parseParameter x
    runGenerator (f p) xs

instance (ReportGenerator a r, ReportParameter p)
      => ReportGenerator (Maybe p -> a) r where
  runGenerator f [] = runGenerator (f Nothing) []
  runGenerator f (x:xs) = do
    p <- parseParameter x
    runGenerator (f $ Just p) xs

class ReportParameter a where
  parseParameter :: (Throws InvalidCmdLine l,
                     Throws InternalError l)
                 => String -> Ledger l a

instance ReportParameter String where
  parseParameter s = return s

instance ReportParameter Path where
  parseParameter s = return (mkPath s)

instance ReportParameter DateTime where
  parseParameter s = do
    now <- wrapIO getCurrentDateTime
    case parseDate now s of
      Right date -> return date
      Left err -> throw (InvalidCmdLine $ s ++ ": " ++ show err)

instance ReportParameter AnyAccount where
  parseParameter s = do
    let path = mkPath s
    getAccount path
      `catchWithSrcLoc`
        (\l (e :: NotAnAccount) ->
             rethrow l (InvalidCmdLine $ show e))
      `catchWithSrcLoc`
        (\l (e :: InvalidPath) ->
             rethrow l (InvalidCmdLine $ show e))

