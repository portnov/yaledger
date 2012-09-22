{-# LANGUAGE TypeFamilies, EmptyDataDecls, MultiParamTypeClasses, FlexibleInstances, RecordWildCards #-}

module YaLedger.Tree where

import Control.Applicative
import Control.Monad
import Data.Either
import Data.List.Utils (split)

import YaLedger.Strings

type Path = [String]

data Tree n a =
    Branch {
      nodeName :: String,
      branchData :: n,
      branchChildren :: Forest n a}
  | Leaf {
      nodeName :: String,
      leafData :: a}

instance (Eq n, Eq a) => Eq (Tree n a) where
  (Branch name n c) == (Branch name' n' c') =
    (name == name') && (n == n') && (c == c')

  (Leaf name a) == (Leaf name' a') =
    (name == name') && (a == a')

  _ == _ = False

showTree :: (Show n, Show a) => Tree n a -> [String]
showTree tree =
    zipS "" (alignMax ALeft $ struct [True] tree)
            (alignMax ARight $ values tree)
  where
    struct (b:bs) (Branch {nodeName = name, branchData = n, branchChildren = children}) =
        (concatMap bar (reverse bs) ++ glyph b ++ name ++ ": "):
         (concatMap (struct (False:b:bs)) $ init children) ++
         (struct (True:b:bs) $ last children)
    struct (b:bs) (Leaf {nodeName = name, leafData = a}) =
        [concatMap bar (reverse bs) ++ glyph b ++ name ++ ": "]

    values (Branch {..}) = show branchData: concatMap values branchChildren
    values (Leaf {..})   = [show leafData]

    bar True  = "  "
    bar False = "| "

    glyph True  = "╰—□ "
    glyph False = "├—□ "

instance (Show n, Show a) => Show (Tree n a) where
  show tree = unlines $ showTree tree

type Forest n a = [Tree n a]

mkPath :: String -> Path
mkPath str = split "/" str

getChildren :: Tree n a -> Forest n a
getChildren (Branch {branchChildren = children}) = children
getChildren (Leaf {}) = []

getData :: Tree n a -> Either n a
getData (Branch {branchData = n}) = Left n
getData (Leaf {leafData = a})     = Right a

leafs :: Tree n a -> [(Path, a)]
leafs tree = go [] tree
  where
    go path (Branch {nodeName = name, branchChildren = children}) =
        concatMap (go (name:path)) children
    go path (Leaf {nodeName = name, leafData = x}) = [(name:path, x)]

leaf :: String -> a -> Tree n a
leaf name x = Leaf name x

branch :: String -> n -> Forest n a -> Tree n a
branch name n list = Branch name n list

fold :: (n -> Forest r a -> r) -> Tree n a -> Tree r a
fold f tree = go tree
  where
      go (Leaf name a)       = Leaf name a
      go (Branch name n lst) = Branch name (f n children) children
        where
          children = map go lst

mapTree  :: (n -> m) -> (a -> b) -> Tree n a -> Tree m b
mapTree bf lf tree = go tree
  where
    go (Branch name n lst) = Branch name (bf n) (map go lst)
    go (Leaf name a) = Leaf name (lf a)

mapLeafsM :: (Monad m, Functor m) => (a -> m b) -> Tree n a -> m (Tree n b)
mapLeafsM f tree = go tree
  where
      go (Leaf name a) = Leaf name <$> f a
      go (Branch name n lst) = do
          Branch name n <$> mapM go lst

forL :: (Monad m) => Tree n a -> (String -> a -> m b) -> m ()
forL tree f = go "" tree
  where
    go path (Leaf {..}) = do
        f (path ++ "/" ++ nodeName) leafData
        return ()
    go path (Branch {..}) =
        forM_ branchChildren $ \tree ->
          go (path ++ "/" ++ nodeName) tree

mapTreeM :: (Monad m, Functor m)
         => (n -> [b] -> m b)
         -> (a -> m b)
         -> Tree n a
         -> m (Tree b b)
mapTreeM foldBranch fn tree = go tree
  where
    go (Leaf name a) = Leaf name <$> fn a
    go (Branch name n children) = do
      res <- mapM go children
      let res' = map getData res
      r <- foldBranch n (lefts res' ++ rights res')
      return $ Branch name r res

filterLeafs :: (a -> Bool) -> Tree n a -> Tree n a
filterLeafs p tree = go tree
  where
    go br@(Branch {})=
      let leafs = [l | l@(Leaf {}) <- branchChildren br, p (leafData l)]
          branches = map go [b | b@(Branch {}) <- branchChildren br]
          branches' = filter (not . null . branchChildren) branches
      in br {branchChildren = leafs ++ branches'}
    go _ = error "Impossible: filterLeafs called on Leaf."

search :: Tree n a -> Path -> [Either n a]
search tree path = map getData $ search' tree path

search' :: Tree n a -> Path -> Forest n a
search' tree [] = [tree]
search' tree path = 
  let name = head path
      ch = getChildren tree
      good = filter ((name ==) . nodeName) ch
      bad  = filter ((name /=) . nodeName) ch
      good' = concatMap (\c -> search' c (tail path)) good
      bad'  = concatMap (\c -> search' c path)        bad
  in  good' ++ bad'

lookupTree :: Path -> Tree n a -> [a]
lookupTree path tree =
  rights $ search tree path

lookupPath :: String -> Tree n a -> [a]
lookupPath path tree = lookupTree (mkPath path) tree

lookupNode :: Path -> Tree n a -> [Tree n a]
lookupNode path tree = 
  case search' tree path of
    [Branch {branchChildren = lst}] -> lst
    _ -> []

changeLeaf :: Tree n a -> Path -> (a -> a) -> Tree n a
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

testTree :: Tree Char Int
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

