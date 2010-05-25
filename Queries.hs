{-# LANGUAGE UnicodeSyntax #-}
module Queries
  (buildCondition)
  where

import Control.Monad
import Data.Maybe

import Types
import Unicode

liftCmp ∷ (Show b) ⇒ ConditionType → (Dated Record → b) → (b → b → Bool) → Maybe b → (Bool → Maybe (ConditionType, Condition))
liftCmp _ _ _ Nothing _ = Nothing
liftCmp t field cmp (Just x) b = Just □ (t, Condition (\y a → (field a) `cmp` y) x b)

buildCondition ∷ Query → Conditions
buildCondition (Q bd ed st) =
  buildConditions □ catMaybes [liftCmp OnStartDate getDate (≥) bd True,
                               liftCmp OnEndDate   getDate (≤) ed True,
                               liftCmp OnStatus    getStatus (≡) st False]
  where
    getStatus (At _ r) = 
      case r of
        PR tr → status tr
        _     → ' '

