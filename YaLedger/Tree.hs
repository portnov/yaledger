{-# LANGUAGE TypeFamilies, EmptyDataDecls, MultiParamTypeClasses, FlexibleInstances, RecordWildCards #-}

module YaLedger.Tree where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Either
import Data.List

import YaLedger.Output.Tables
import YaLedger.Output.ASCII

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
    zipS "" (alignMax ALeft $ showTreeStructure tree)
            (alignMax ARight $ values tree)
  where
    values (Branch {..}) = show branchData: concatMap values branchChildren
    values (Leaf {..})   = [show leafData]

showTreeStructure :: (Show n, Show a) => Tree n a -> [String]
showTreeStructure tree = struct [True] tree
  where
    struct (b:bs) (Branch {nodeName = name, branchData = n, branchChildren = []}) =
        [concatMap bar (reverse bs) ++ glyph b ++ name ++ ": ∅"]
    struct (b:bs) (Branch {nodeName = name, branchData = n, branchChildren = children}) =
        (concatMap bar (reverse bs) ++ glyph b ++ name ++ ": "):
         (concatMap (struct (False:b:bs)) $ init children) ++
         (struct (True:b:bs) $ last children)
    struct (b:bs) (Leaf {nodeName = name, leafData = a}) =
        [concatMap bar (reverse bs) ++ glyph b ++ name ++ ": "]

    bar True  = "  "
    bar False = "| "

    glyph True  = "╰—□ "
    glyph False = "├—□ "

instance (Show n, Show a) => Show (Tree n a) where
  show tree = unlines $ showTree tree

type Forest n a = [Tree n a]

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split c str =
  case break (== c) str of
    (rem,[]) -> [rem]
    (x, _:xs) -> x: split c xs

mkPath :: String -> Path
mkPath str = split '/' str

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

-- | Apply pure functions to all nodes of tree.
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

allNodes :: Tree a a -> [a]
allNodes (Leaf {..}) = [leafData]
allNodes (Branch {..}) =
    branchData: concatMap allNodes branchChildren

allLeafs :: Tree n a -> [a]
allLeafs (Leaf {..}) = [leafData]
allLeafs (Branch {..}) =
    concatMap allLeafs branchChildren

allPaths :: Tree n a -> [Path]
allPaths (Leaf {..}) = [[nodeName]]
allPaths (Branch {..}) =
   [nodeName]: map (nodeName:) (concatMap allPaths branchChildren)

allLeafPaths :: Tree n a -> [Path]
allLeafPaths (Leaf {..}) = [[nodeName]]
allLeafPaths (Branch {..}) =
   map (nodeName:) (concatMap allLeafPaths branchChildren)

-- | Similar to 'forM_', but iterates on all leafs of tree.
forL :: (Monad m) => Tree n a -> (String -> a -> m b) -> m ()
forL tree f = go "" tree
  where
    go path (Leaf {..}) = do
        f (path ++ "/" ++ nodeName) leafData
        return ()
    go path (Branch {..}) =
        forM_ branchChildren $ \tree ->
          go (path ++ "/" ++ nodeName) tree

-- | Apply monadic functions to branches and leafs of tree.
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
    go l@(Leaf {})
      | p (leafData l) = l
      | otherwise = error "filterLeafs called on bad Leaf."

-- | Search all sub-trees matching with given part of path.
-- For example, \/A\/B\/C\/D can be reached as
-- `D', `C\/D', 'A\/D', 'A\/C\/D', and so on.
search' :: Tree n a -> Path -> Forest n a
search' tree [] = [tree]
search' tree path =
    let allVariants = go [] True tree path
    in case filter (\(x,_,_) -> x) allVariants of
         [] -> let paths = [p | (_,p,_) <- allVariants]
               in  [x | (_,_,x) <- allVariants]
         [(_,p,x)] -> [x]
         _ -> error $ "Internal error: search': ambigous exact path: " ++ intercalate "/" path
  where
    click _ = False

    go :: Path -> Bool -> Tree n a -> Path -> [(Bool, Path, Tree n a)]
    go q b tree [] = [(b, (nodeName tree: q), tree)]
    go q b tree [name]
      | nodeName tree == name = [(b, (name: q), tree)]
      | otherwise = concat [go (nodeName tree: q) (click b) c [name] | c <- getChildren tree]
    go q b tree (p:ps)
      | nodeName tree == p = concat [go (p:q) b c ps | c <- getChildren tree]
      | otherwise = concat [go (nodeName tree:q) (click b) c (p:ps) | c <- getChildren tree]

-- | Search leaf or branch by given part of path.
-- See 'search\''.
search :: Tree n a -> Path -> [Either n a]
search tree path = map getData $ search' tree path

-- | Search a sub-tree by exact path
searchExactly :: Tree n a -> Path -> Maybe (Tree n a)
searchExactly tree [] = Just tree
searchExactly tree (p:ps)
  | nodeName tree == p =
    case catMaybes [searchExactly c ps | c <- getChildren tree] of
      [] -> Nothing
      [x] -> Just x
      _ -> error "Internal error: searchExactly: ambigous."
  | otherwise = Nothing

-- | Look up for a leaf by part of path.
lookupTree :: Path -> Tree n a -> [a]
lookupTree path tree =
  rights $ search tree path

-- | Similar to 'lookupPath', but with String
-- instead of Path.
lookupPath :: String -> Tree n a -> [a]
lookupPath path tree = lookupTree (mkPath path) tree

lookupNode :: Path -> Tree n a -> [Tree n a]
lookupNode path tree = 
  case search' tree path of
    [Branch {branchChildren = lst}] -> lst
    _ -> []

-- | Change one leaf (specified with partial path) using
-- pure function. NB: node specification should be unambigous.
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

