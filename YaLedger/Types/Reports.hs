{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeOperators, ScopedTypeVariables, ExistentialQuantification, OverlappingInstances, TypeFamilies #-}
-- | This module contains basic interfaces descriptions for reports.
module YaLedger.Types.Reports
  (ReportClass (..), ReportParameter (..),
   Report (..),
   OptDescr (..), ArgDescr (..),
   runAReport
  ) where

import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Loc
import Data.Dates
import System.Console.GetOpt

import YaLedger.Tree
import YaLedger.Exceptions
import YaLedger.Types.Ledger
import YaLedger.Types.Transactions
import YaLedger.Monad
import YaLedger.Kernel.Common

-- | Simple command line parser monad
newtype Parser l a = Parser {
    runParser :: [String] -> Ledger l (a, [String])
  }

-- | Trivial instance
instance Monad (Parser l) where
  return x = Parser (\s -> return (x, s))

  m >>= f = Parser $ \s -> do
    (y, s') <- runParser m s
    runParser (f y) s'

-- | Trivial instance
instance Functor (Parser l) where
  fmap = liftM

-- | This is similar to Control.Monad.Trans.lift,
-- but with more precise type.
liftL :: Ledger l a -> Parser l a
liftL action = Parser $ \s -> do
  y <- action
  return (y, s)

-- | Get one word from command line
word :: Throws InvalidCmdLine l => Parser l String
word = Parser $ \s ->
         case s of
           [] -> throw (InvalidCmdLine "Not enough parameters")
           (x:xs) -> return (x, xs)

-- | Parseable report command-line parameter
class ReportParameter a where
  parseParameter :: (Throws InvalidCmdLine l,
                     Throws InternalError l)
                 => Parser l a

class (ReportParameter (Parameters a)) => ReportClass a where
  -- | Report options (like @-z@), parsed with GetOpt
  type Options a
  -- | Report parameters, parsed with report's specific parser
  type Parameters a

  -- | GetOpt options
  reportOptions :: a -> [OptDescr (Options a)]
  reportOptions _ = []

  -- | Default set of options
  defaultOptions :: a -> [Options a]
  defaultOptions _ = []

  -- | Main function of this class.
  -- It usually outputs report to stdout.
  runReport :: (Throws InvalidCmdLine l,
                Throws InternalError l)
            => a             -- ^ Report itself; usually this is a dummy parameter, for typechecker only
            -> Query         -- ^ Which records to process
            -> [Options a]   -- ^ Report options, parsed by GetOpt
            -> Parameters a  -- ^ Report parameters, parsed by report's specific parser
            -> Ledger l ()

  -- | Description of report and it's parameters/options.
  reportHelp :: a -> String

-- | Container type for any report.
data Report =
  forall a. (ReportClass a) => Report a

instance ReportParameter () where
  parseParameter = return ()

instance ReportParameter a => ReportParameter (Maybe a) where
  parseParameter = Parser $ \s ->
    case s of
      [] -> return (Nothing, [])
      (x:xs) -> do
          (r,s) <- runParser parseParameter (x:xs)
          return (Just r, s)

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

-- | Run any report
runAReport :: (Throws InvalidCmdLine l,
               Throws InternalError l)
           => Query        -- ^ Which records to process
           -> [String]     -- ^ Report's command line
           -> Report       -- ^ Report itself
           -> Ledger l ()
runAReport qry cmdline (Report r) = do
  let ropts = reportOptions r
  (options, params) <- do
    if null ropts
      then do
           p <- runParser parseParameter cmdline
           return (defaultOptions r, p)
      else case getOpt RequireOrder ropts cmdline of
             (opts, ps, []) -> do
                 p <- runParser parseParameter ps
                 return (opts, p)
             (_,_, errs) -> do
               let message = usageInfo (reportHelp r) ropts
               throw (InvalidCmdLine $ concat errs ++ message)
  runReport r qry options (fst params)

