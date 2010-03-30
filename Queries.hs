module Queries
  (buildQuery)
  where

import Control.Monad

import Types

liftCmp :: (a -> b) -> (b -> b -> Bool) -> Maybe b -> (a -> Bool)
liftCmp _ _ Nothing = const True
liftCmp field cmp (Just x) = \a -> (field a) `cmp` x

buildQuery :: Query -> (Dated Record -> Bool)
buildQuery (Q bd ed st) =
  foldr (liftM2 (&&)) (const True) [liftCmp getDate (>=) bd,
                                    liftCmp getDate (<=) ed,
                                    liftCmp getStatus (==) st]
  where
    getStatus (At _ r) = 
      case r of
        PR tr -> status tr
        _     -> ' '
