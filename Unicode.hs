module Unicode where

type ℝ = Float
type ℤ = Integer
type 𝔹 = Bool

(⧺) = (++)
(□) = ($)
infixr 0 □
(∘) = (.)
(∨) = (||)
(∧) = (&&)
x ∈ lst = elem x lst

(≥) :: (Ord a) => a -> a -> Bool
(≥) = (>=)
(≤) :: (Ord a) => a -> a -> Bool
(≤) = (<=)

(≡) :: (Eq a) => a -> a -> Bool
(≡) = (==)

(≠) :: (Eq a) => a -> a -> Bool
(≠) = (/=)


