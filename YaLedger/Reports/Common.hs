{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeOperators, ScopedTypeVariables, ExistentialQuantification #-}
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
module YaLedger.Reports.Common where

import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Data.Dates

import YaLedger.Types
import YaLedger.Exceptions
import YaLedger.Monad

class ReportGenerator a r where
  runGenerator :: (Throws InvalidCmdLine l,
                   Throws InternalError l)
               => a -> [String] -> Ledger l r

data Report r = forall a. ReportGenerator a r => Report a

instance ReportGenerator r r where
  runGenerator r [] = return r
  runGenerator _ lst = throw (InvalidCmdLine $ "Too many parameters: " ++ unwords lst)

instance (ReportGenerator a r, IsReportParameter p)
      => ReportGenerator (p -> a) r where
  runGenerator f (x:xs) = do
    p <- parseParameter x
    runGenerator (f p) xs

class IsReportParameter a where
  parseParameter :: (Throws InvalidCmdLine l,
                     Throws InternalError l)
                 => String -> Ledger l a

instance IsReportParameter String where
  parseParameter s = return s

instance IsReportParameter Path where
  parseParameter s = return (mkPath s)

instance IsReportParameter DateTime where
  parseParameter s = do
    now <- wrapIO getCurrentDateTime
    case parseDate now s of
      Right date -> return date
      Left err -> throw (InvalidCmdLine $ s ++ ": " ++ show err)

instance IsReportParameter AnyAccount where
  parseParameter s = do
    let path = mkPath s
    plan <- gets lsAccountPlan 
    case lookupTree path plan of
      [] -> throw $ InvalidCmdLine $ "No such account: " ++ s
      [x] -> return x
      _ -> throw $ InvalidCmdLine $ "Ambigous account specification: " ++ s

