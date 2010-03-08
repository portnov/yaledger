{-# LANGUAGE DeriveDataTypeable #-}

module Tree
  where

import Prelude hiding (lookup)
import Data.Generics

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
                      


testTree :: Tree Int Char
testTree = Node "root" 1 $ [
  Node "2" 2 $ [
    Node "3" 3 [Leaf "L1" 'A', Leaf "L2" 'B', Leaf "L3" 'C'],
    Node "Node" 4 $ [
      Leaf "Leaf" 'D',
      Node "5" 5 [Leaf "Leaf" 'E'],
      Leaf "L4" 'F' ]],
  Node "Node" 17 $ [
    Leaf "L5" 'G',
    Node "Leaf" 6 [Leaf "L6" 'H']]]
