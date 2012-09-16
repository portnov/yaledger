{-# LANGUAGE TypeFamilies, EmptyDataDecls, MultiParamTypeClasses, FlexibleInstances #-}

module YaLedger.Tree where

import Control.Applicative
import Data.Either
import Data.List.Utils (split)

data family ParentLink s x

data Linked
data NotLinked

data instance ParentLink Linked a =
    Root
  | ParentLink a
  deriving (Eq)

instance Monad (ParentLink Linked) where
  return x = ParentLink x
  m >>= f =
    case m of
      Root -> Root
      ParentLink x -> f x

data instance ParentLink NotLinked a = NoLink
  deriving (Eq)

type Path = [String]

data Tree s n a =
    Branch {
      nodeParent :: ParentLink s (Tree s n a),
      nodeName :: String,
      branchData :: n,
      branchChildren :: Forest s n a}
  | Leaf {
      nodeParent :: ParentLink s (Tree s n a),
      nodeName :: String,
      leafData :: a}

instance (Eq n, Eq a) => Eq (Tree s n a) where
  (Branch _ name n c) == (Branch _ name' n' c') =
    (name == name') && (n == n') && (c == c')

  (Leaf _ name a) == (Leaf _ name' a') =
    (name == name') && (a == a')

  _ == _ = False

instance (Show n, Show a) => Show (Tree s n a) where
  show tree = go 0 False tree
    where
      go i b (Branch {nodeName = name, branchData = n, branchChildren = children}) =
          (concat $ replicate i "│ ") ++ 
          (glyph b) ++ name ++ ":\t" ++ show n ++ "\n" ++
          (concatMap (go (i+1) False) $ init children) ++
          (go (i+1) True $ last children)
      go i b (Leaf {nodeName = name, leafData = a}) =
          (concat $ replicate i "│ ") ++
          (glyph b) ++ name ++ ":\t" ++ show a ++ "\n"

      glyph True  = "╰—□ "
      glyph False = "├—□ "

type Forest s n a = [Tree s n a]

mkPath :: String -> Path
mkPath str = split "/" str

getChildren :: Tree s n a -> Forest s n a
getChildren (Branch {branchChildren = children}) = children
getChildren (Leaf {}) = []

getData :: Tree s n a -> Either n a
getData (Branch {branchData = n}) = Left n
getData (Leaf {leafData = a})     = Right a

leafs :: Tree s n a -> [(Path, a)]
leafs tree = go [] tree
  where
    go path (Branch {nodeName = name, branchChildren = children}) =
        concatMap (go (name:path)) children
    go path (Leaf {nodeName = name, leafData = x}) = [(name:path, x)]

leaf :: String -> a -> Tree Linked n a
leaf name x = Leaf Root name x

branch :: String -> n -> Forest Linked n a -> Tree Linked n a
branch name n list =
  let result = Branch Root name n children
      children = [c {nodeParent = ParentLink result} | c <- list]
  in  result

fold :: (n -> Forest Linked r a -> r) -> Tree Linked n a -> Tree Linked r a
fold f tree =
  let result = go Root tree
      
      go p (Leaf _ name a)       = Leaf p name a
      go p (Branch _ name n lst) = Branch p name (f n children) children
        where
          children = map (go $ ParentLink result) lst

  in  result

mapLeafsM :: (Monad m, Functor m) => (a -> m b) -> Tree s n a -> m (Tree NotLinked n b)
mapLeafsM f tree = go tree
  where
      go (Leaf _ name a) = Leaf NoLink name <$> f a
      go (Branch _ name n lst) = do
          Branch NoLink name n <$> mapM go lst

mapTreeM :: (Monad m, Functor m)
         => (n -> [b] -> m b)
         -> (a -> m b)
         -> Tree s n a
         -> m (Tree NotLinked b b)
mapTreeM foldBranch fn tree = go tree
  where
    go (Leaf _ name a) = Leaf NoLink name <$> fn a
    go (Branch _ name n children) = do
      res <- mapM go children
      let res' = map getData res
      r <- foldBranch n (lefts res' ++ rights res')
      return $ Branch NoLink name r res

search :: Tree s n a -> Path -> [Either n a]
search tree path = map getData $ search' tree path

search' :: Tree s n a -> Path -> Forest s n a
search' tree [] = [tree]
search' tree path = 
  let name = head path
      ch = getChildren tree
      good = filter ((name ==) . nodeName) ch
      bad  = filter ((name /=) . nodeName) ch
      good' = concatMap (\c -> search' c (tail path)) good
      bad'  = concatMap (\c -> search' c path)        bad
  in  good' ++ bad'

lookupTree :: Path -> Tree s n a -> [a]
lookupTree path tree =
  rights $ search tree path

lookupPath :: String -> Tree s n a -> [a]
lookupPath path tree = lookupTree (mkPath path) tree

lookupNode :: Path -> Tree s n a -> [Tree s n a]
lookupNode path tree = 
  case search' tree path of
    [Branch {branchChildren = lst}] -> lst
    _ -> []

getAttribute :: (n -> Maybe r) -> (a -> Maybe r) -> Tree Linked n a -> Maybe r
getAttribute checkBranch checkLeaf tree =
  case getData tree of
    Left n  -> case checkBranch n of
                Nothing -> case nodeParent tree of
                             Root -> Nothing
                             ParentLink p -> getAttribute checkBranch checkLeaf p
                Just val -> return val
    Right a -> case checkLeaf a of
                Nothing ->  case nodeParent tree of
                             Root -> Nothing
                             ParentLink p -> getAttribute checkBranch checkLeaf p
                Just val -> return val

changeLeaf :: Tree s n a -> Path -> (a -> a) -> Tree s n a
changeLeaf tree path f =
    case search tree path of
      [Right _] -> changed tree path
      _         -> tree
  where
    changed tree [] = tree
    changed node@(Branch {nodeName = name, branchChildren = list}) (p:ps)
        | p == name = node {branchChildren = map (\c -> changed c ps) list}
        | otherwise = node {branchChildren = map (\c -> changed c (p:ps)) list}
    changed leaf@(Leaf {nodeName = name}) [p]
        | p == name = leaf {leafData = f (leafData leaf) }
        | otherwise = leaf
    changed leaf@(Leaf {}) _ = leaf

testTree :: Tree Linked Char Int
testTree = branch "root" 'R' $ [
  branch "2" 'A' $ [
    branch "3" 'B' [leaf "L1" 1, leaf "L2" 2, leaf "L3" 3],
    branch "Node" 'C' $ [
      leaf "Leaf" 4,
      branch "5" 'D' [leaf "Leaf" 5],
      leaf "L4" 6 ]],
  branch "Node" 'E' $ [
    leaf "L5" 7,
    branch "Leaf" 'F' [leaf "L6" 8]]]

