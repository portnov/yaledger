{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, GADTs #-}

module YaLedger.Processor.Templates
  (SubstState, Subst,
   ATemplate (..),
   TemplateMonad (..),
   fillTemplate,
   getTemplate
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Exception
import Control.Concurrent.STM
import qualified Data.Map as M
import Data.Maybe
import Data.Decimal

import YaLedger.Types
import YaLedger.Exceptions
import YaLedger.Kernel (getCurrentBalance)

class Monad m => TemplateMonad m where
  liftTemplate :: Throws InternalError l => LedgerT l STM a -> LedgerT l m a

instance TemplateMonad IO where
  liftTemplate = runAtomically

instance TemplateMonad STM where
  liftTemplate = id

type SubstState = M.Map Int Amount

type Subst l m a = ReaderT SubstState (EMT l (LedgerStateT m)) a

use :: Monad m => Int -> Subst l m (Maybe Amount)
use i = asks (M.lookup i)

class ATemplate a where
  type Result a

  nParams :: a -> Int
  subst :: (TemplateMonad m, Throws InternalError l) => a -> Subst l m (Result a)

instance ATemplate a => ATemplate (Ext a) where
  type Result (Ext a) = Ext (Result a)

  nParams (Ext _ _ _ _ a) = nParams a

  subst (Ext date i loc attrs a) = Ext date i loc attrs <$> subst a 

instance ATemplate (Transaction Param) where
  type Result (Transaction Param) = Transaction Amount

  nParams (TEntry entry) = nParams entry
  nParams (TReconciliate _ a _ _) = nParams a
  nParams _ = 0

  subst (TEntry entry) = TEntry <$> subst entry
  subst (TReconciliate p a tgt msg) = (\x -> TReconciliate p x tgt msg) <$> subst a
  subst (TCallTemplate n t) = return $ TCallTemplate n t

instance ATemplate (Entry Param Unchecked) where
  type Result (Entry Param Unchecked) = Entry Amount Unchecked

  nParams (UEntry dt cr _ _) =
    let ms x = if null x then 0 else maximum x
    in ms (map nParams dt) `max` ms (map nParams cr)

  subst (UEntry dt cr a cs) = UEntry <$> mapM subst dt <*> mapM subst cr <*> return a <*> return cs

instance ATemplate Param where
  type Result Param = Amount

  nParams _ = 1

  subst (Fixed x) = return x
  subst (Param i c d) = do
    mbX <- use i
    let (x :# cur) = fromMaybe d mbX
    return $ (x *. c) :# cur
  subst x = fail $ "Internal error: subst " ++ show x

instance ATemplate (Posting Param t) where
  type Result (Posting Param t) = Posting Amount t

  nParams (DPosting _ a _) = nParams a
  nParams (CPosting _ a _) = nParams a

  subst (DPosting acc a b) =
    case a of
      FromBalance c -> do
        balance <- lift $ liftTemplate (getCurrentBalance AvailableBalance acc)
        return $ DPosting acc (balance *. c :# getCurrency acc) b
      _ -> DPosting acc <$> subst a <*> return b
  subst (CPosting acc a b) =
    case a of
      FromBalance c -> do
        balance <- lift $ liftTemplate (getCurrentBalance AvailableBalance acc)
        return $ CPosting acc (balance *. c :# getCurrency acc) b
      _ -> CPosting acc <$> subst a <*> return b

fillTemplate :: (TemplateMonad m, Throws InternalError l)
              => Transaction Param
              -> [Amount]
              -> LedgerT l m (Transaction Amount)
fillTemplate tran args =
  runReaderT (subst tran) (M.fromList $ zip [1..] args)

getTemplate :: (Throws NoSuchTemplate l,
                Throws InternalError l)
            => String
            -> Ledger l (Attributes, Transaction Param)
getTemplate name = do
  tpls <- gets lsTemplates
  case M.lookup name tpls of
    Nothing -> throwP (NoSuchTemplate name)
    Just tpl -> return tpl

