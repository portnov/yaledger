{-# LANGUAGE UnicodeSyntax, RelaxedPolyRec #-}

module Lists where

import Unicode

import Data.Function
import Data.List

minimumNBy ∷ (a → a → Ordering) → [a] → Maybe (Int,a)
minimumNBy compare lst =
  case lst of
    [] → Nothing
    lst' → Just □ minimumBy (compare `on` snd) □ zip [1..] lst'

tailN ∷ Int → [[a]] → [[a]]
tailN n lists = map tail' zipped
  where
    zipped = zip [1..] lists
    tail' (i,lst) | i ≡ n    = drop 1 lst
                  | otherwise = lst

mergeBy ∷ (a → a → Ordering) → [[a]] → [a]
mergeBy compare lists =
  case minimumNBy compare □ concatMap (take 1) lists of
    Nothing → []
    Just (n,m) → m : mergeBy compare (tailN n □ filter (not ∘ null) lists)

merge ∷ (Ord a) ⇒ [[a]] → [a]
merge = mergeBy compare

mergeOn ∷ (Ord b) ⇒ (a → b) → [[a]] → [a]
mergeOn fn = mergeBy (compare `on` fn)

isSorted ∷ (Ord a) ⇒ [a] → Bool
isSorted list = list ≡ sort list


l1 = [1,7..]
l2 = [3,9..]
l3 = [4,8..]

lst = [l1,l2,l3]
