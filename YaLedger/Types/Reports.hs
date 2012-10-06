{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeOperators, ScopedTypeVariables, ExistentialQuantification, OverlappingInstances, TypeFamilies #-}
{- # OPTIONS_GHC -F -pgmF MonadLoc #-}
module YaLedger.Types.Reports
  (ReportClass (..), ReportParameter (..),
   Report (..),
   OptDescr (..),
   runAReport
  ) where

import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Exception.Base
import Control.Monad.Loc
import Data.Dates
import System.Console.GetOpt

import YaLedger.Tree
import YaLedger.Exceptions
import YaLedger.Types.Ledger
import YaLedger.Types.Transactions
import YaLedger.Monad
import YaLedger.Kernel.Common

newtype Parser l a = Parser {
    runParser :: [String] -> Ledger l (a, [String])
  }

instance Monad (Parser l) where
  return x = Parser (\s -> return (x, s))

  m >>= f = Parser $ \s -> do
    (y, s') <- runParser m s
    runParser (f y) s'

instance Functor (Parser l) where
  fmap = liftM

liftL :: Ledger l a -> Parser l a
liftL action = Parser $ \s -> do
  y <- action
  return (y, s)

word :: Throws InvalidCmdLine l => Parser l String
word = Parser $ \s ->
         case s of
           [] -> throw (InvalidCmdLine "Not enough parameters")
           (x:xs) -> return (x, xs)

maybeWord :: Parser l (Maybe String)
maybeWord = Parser $ \s ->
              case s of
                [] -> return (Nothing, [])
                (x:xs) -> return (Just x, xs)

class ReportParameter a where
  parseParameter :: (Throws InvalidCmdLine l,
                     Throws InternalError l)
                 => Parser l a

class (ReportParameter (Parameters a)) => ReportClass a where
  type Options a
  type Parameters a

  reportOptions :: a -> [OptDescr (Options a)]
  defaultOptions :: a -> [Options a]
  runReport :: (Throws InvalidCmdLine l,
                Throws InternalError l)
            => a -> Query -> [Options a] -> Parameters a -> Ledger l ()
  reportHelp :: a -> String

data Report =
  forall a. (ReportClass a) => Report a

instance ReportParameter () where
  parseParameter = return ()

instance ReportParameter a => ReportParameter (Maybe a) where
  parseParameter = Parser $ \s ->
    case s of
      [] -> return (Nothing, [])
      (x:xs) -> runParser parseParameter (x:xs)

instance ReportParameter String where
  parseParameter  = word

instance ReportParameter Path where
  parseParameter = mkPath <$> word

instance ReportParameter DateTime where
  parseParameter = do
    s <- word
    now <- liftL (gets lsStartDate)
    case parseDate now s of
      Right date -> return date
      Left err -> liftL $ throw (InvalidCmdLine $ s ++ ": " ++ show err)

instance ReportParameter AnyAccount where
  parseParameter = do
    s <- word
    liftL $ 
      getAccount (gets lsPosition) (gets lsCoA) (mkPath s)
        `catchWithSrcLoc`
          (\l (e :: NotAnAccount) ->
               rethrow l (InvalidCmdLine $ show e))
        `catchWithSrcLoc`
          (\l (e :: InvalidPath) ->
               rethrow l (InvalidCmdLine $ show e))

runAReport :: (Throws InvalidCmdLine l,
               Throws InternalError l)
           => Query -> [String] -> Report -> Ledger l ()
runAReport qry cmdline (Report r) = do
  (options, params) <- do
    if null (reportOptions r)
      then do
           p <- runParser parseParameter cmdline
           return (defaultOptions r, p)
      else case getOpt RequireOrder (reportOptions r) cmdline of
             (opts, ps, []) -> do
                 p <- runParser parseParameter ps
                 return (opts, p)
             (_,_, errs) -> throw (InvalidCmdLine $ concat errs)
  runReport r qry options (fst params)

