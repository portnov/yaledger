module Queries
  (buildCondition)
  where

import Control.Monad
import Data.Maybe

import Types

liftCmp :: (Show b) => (Dated Record -> b) -> (b -> b -> Bool) -> Maybe b -> (Bool -> Maybe Condition)
liftCmp _ _ Nothing _ = Nothing
liftCmp field cmp (Just x) b = Just $ Condition (\y a -> (field a) `cmp` y) x b

buildCondition :: Query -> [Condition]
buildCondition (Q bd ed st) = catMaybes [liftCmp getDate (>=) bd True,
                                        liftCmp getDate (<=) ed True,
                                        liftCmp getStatus (==) st False]
  where
    getStatus (At _ r) = 
      case r of
        PR tr -> status tr
        _     -> ' '

