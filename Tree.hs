{-# LANGUAGE DeriveDataTypeable #-}

module Tree
  where

import Prelude hiding (lookup)
import Data.Generics
import Data.List.Utils (split)

data Tree n a =
      Node {getName :: String, nodeData :: n, children :: [Tree n a]}
    | Leaf {getName :: String, leafData :: a}
  deriving (Eq, Data, Typeable)

getData :: Tree n a -> Either n a
getData (Node _ n _) = Left n
getData (Leaf _ a)   = Right a

getChildren :: Tree n a -> [Tree n a]
getChildren (Node _ _ c) = c
getChildren (Leaf _ _)   = []

leafs :: Tree n a -> [a]
leafs (Node _ _ lst) = concatMap leafs lst
leafs (Leaf _ a) = [a]

search :: Tree n a -> [String] -> [Either n a]
search tree path = map getData $ search' tree path

search' :: Tree n a -> [String] -> [Tree n a]
search' tree [] = [tree]
search' tree path = 
  let name = head path
      ch = getChildren tree
      good = filter ((name ==) . getName) ch
      bad  = filter ((name /=) . getName) ch
      good' = concatMap (\c -> search' c (tail path)) good
      bad'  = concatMap (\c -> search' c path)        bad
  in  good' ++ bad'

lookup :: [String] -> Tree n a -> Maybe a
lookup path tree =
  case search tree path of
    [Right x] -> Just x  
    _         -> Nothing

lookupPath :: String -> Tree n a -> Maybe a
lookupPath path tree = lookup (split "/" path) tree

changeLeaf :: Tree n a -> [String] -> a -> Tree n a
changeLeaf tree path new =
    case search tree path of
      [Right _] -> changed tree path
      _         -> tree
  where
    changed tree [] = tree
    changed node@(Node name n lst) (p:ps) | p == name = Node name n $ map (\c -> changed c ps) lst
                                          | otherwise = Node name n $ map (\c -> changed c (p:ps)) lst
    changed leaf@(Leaf name x) [p] | p == name = Leaf name new
                                   | otherwise = leaf
    changed leaf@(Leaf name x) _ = leaf
                      
sumTree :: (Num a) => Tree n a -> Tree (a,n) a
sumTree (Leaf name x) = Leaf name x
sumTree (Node name n lst) = Node name (s,n) lst'
  where
    lst' = map sumTree lst
    leafs = filter isLeaf lst'
    nodes = filter (not . isLeaf) lst'
    s = sum [x | Leaf _ x <- leafs]
      + sum [x | Node _ (x,_) _ <- nodes]
    isLeaf (Leaf _ _) = True
    isLeaf _          = False

partFold :: (n -> [a] -> s) -> (s -> s -> s) -> ([s] -> s) -> Tree n a -> Tree s a
partFold _ _ _ (Leaf name x) = Leaf name x
partFold foldA plus foldS (Node name n lst) = Node name s lst'
  where
    lst' = map (partFold foldA plus foldS) lst
    leafs = filter isLeaf lst'
    nodes = filter (not . isLeaf) lst'
    s = foldA n [x | Leaf _ x <- leafs]
      `plus` foldS [x | Node _ x _ <- nodes]
    isLeaf (Leaf _ _) = True
    isLeaf _          = False

showTree :: (Show n, Show a) => Int -> Tree n a -> String
showTree k (Leaf name a) = (replicate k ' ') ++ name ++ ": " ++ show a
showTree k (Node name n lst) = (replicate k ' ') ++ name ++ ": " ++ show n ++ "\n" ++ (unlines $ map (showTree $ k+2) lst)

testTree :: Tree Char Int
testTree = Node "root" 'R' $ [
  Node "2" 'A' $ [
    Node "3" 'B' [Leaf "L1" 1, Leaf "L2" 2, Leaf "L3" 3],
    Node "Node" 'C' $ [
      Leaf "Leaf" 4,
      Node "5" 'D' [Leaf "Leaf" 5],
      Leaf "L4" 6 ]],
  Node "Node" 'E' $ [
    Leaf "L5" 7,
    Node "Leaf" 'F' [Leaf "L6" 8]]]
