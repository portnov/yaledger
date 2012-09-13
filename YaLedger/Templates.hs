{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, GADTs #-}

module YaLedger.Templates where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Exception
import qualified Data.Map as M
import Data.Maybe
import Data.Decimal
import Text.Printf

import YaLedger.Types
import YaLedger.Exceptions
import YaLedger.Monad

type SubstState = M.Map Int Amount

type Subst a = Reader SubstState a

use :: Int -> Subst (Maybe Amount)
use i = asks (M.lookup i)

class ATemplate a where
  type Result a

  nParams :: a -> Int
  subst :: a -> Subst (Result a)

instance ATemplate a => ATemplate (Ext a) where
  type Result (Ext a) = Ext (Result a)

  nParams (Ext _ _ a) = nParams a

  subst (Ext date attrs a) = Ext date attrs <$> subst a 

instance ATemplate (Transaction Param) where
  type Result (Transaction Param) = Transaction Amount

  nParams (TEntry entry) = nParams entry
  nParams (TReconciliate _ a) = nParams a
  nParams (TInitlalize _ a) = nParams a
  nParams _ = 0

  subst (TEntry entry) = TEntry <$> subst entry
  subst (TReconciliate p a) = TReconciliate p <$> subst a
  subst (TInitlalize p a) = TInitlalize p <$> subst a
  subst (TCallTemplate n t) = return $ TCallTemplate n t
  subst (TSetRate c1 c2 x)  = return $ TSetRate c1 c2 x

instance ATemplate (Entry Param Unchecked) where
  type Result (Entry Param Unchecked) = Entry Amount Unchecked

  nParams (UEntry dt cr _ _) = sum (map nParams dt) + sum (map nParams cr)

  subst (UEntry dt cr a cs) = UEntry <$> mapM subst dt <*> mapM subst cr <*> return a <*> return cs

instance ATemplate Param where
  type Result Param = Amount

  nParams _ = 1

  subst (Fixed x) = return x
  subst (Param i c d) = do
    mbX <- use i
    let (x :# cur) = fromMaybe d mbX
    return $ (x *. c) :# cur

instance ATemplate (Posting Param t) where
  type Result (Posting Param t) = Posting Amount t

  nParams (DPosting _ a) = nParams a
  nParams (CPosting _ a) = nParams a

  subst (DPosting acc a) = DPosting acc <$> subst a
  subst (CPosting acc a) = CPosting acc <$> subst a

fillTemplate :: Transaction Param -> [Amount] -> Ledger l (Transaction Amount)
fillTemplate tran args =
  return $ runReader (subst tran) (M.fromList $ zip [1..] args)

getTemplate :: (Throws NoSuchTemplate l)
            => String
            -> Ledger l (Attributes, Transaction Param)
getTemplate name = do
  tpls <- gets lsTemplates
  case M.lookup name tpls of
    Nothing -> throw (NoSuchTemplate name)
    Just tpl -> return tpl

