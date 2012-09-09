module Tree where

import Control.Applicative
import Control.Monad
import Data.Either
import Data.List.Utils (split)

type Path = [String]

data Tree n a =
    Branch {
      nodeParent :: Maybe (Tree n a),
      nodeName :: String,
      branchData :: n,
      branchChildren :: Forest n a}
  | Leaf {
      nodeParent :: Maybe (Tree n a),
      nodeName :: String,
      leafData :: a}
  deriving (Eq)

instance (Show n, Show a) => Show (Tree n a) where
  show tree = go 0 False tree
    where
      go i b (Branch {nodeName = name, branchData = n, branchChildren = children}) =
          (concat $ replicate i "│ ") ++ 
          (glyph b) ++ name ++ ": " ++ (show n) ++ "\n" ++
          (concatMap (go (i+1) False) $ init children) ++
          (go (i+1) True $ last children)
      go i b (Leaf {nodeName = name, leafData = a}) =
          (concat $ replicate i "│ ") ++
          (glyph b) ++ name ++ ": " ++ (show a) ++ "\n"

      glyph True  = "╰—"
      glyph False = "├—"

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
leaf name x = Leaf Nothing name x

branch :: String -> n -> Forest n a -> Tree n a
branch name n list =
  let result = Branch Nothing name n children
      children = [c {nodeParent = Just result} | c <- list]
  in  result

fold :: (n -> Forest r a -> r) -> Tree n a -> Tree r a
fold f tree =
  let result = go Nothing tree
      
      go p (Leaf _ name a)       = Leaf p name a
      go p (Branch _ name n lst) = Branch p name (f n children) children
        where
          children = map (go $ Just result) lst

  in  result

mapLeafsM :: (Monad m, Functor m) => (a -> m b) -> Tree n a -> m (Tree n b)
mapLeafsM f tree =
  let result = go Nothing tree

      go p (Leaf _ name a) = Leaf p name <$> f a
      go p (Branch _ name n lst) = do
          res <- result
          Branch p name n <$> mapM (go $ Just res) lst

  in  result

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

lookupNode :: String -> Tree n a -> [Tree n a]
lookupNode path tree = 
  case search' tree (mkPath path) of
    [Branch {branchChildren = lst}] -> lst
    _ -> []

getAttribute :: (n -> Maybe r) -> (a -> Maybe r) -> Tree n a -> Maybe r
getAttribute checkBranch checkLeaf tree =
  case getData tree of
    Left n  -> case checkBranch n of
                Nothing -> getAttribute checkBranch checkLeaf =<< nodeParent tree
                Just val -> return val
    Right a -> case checkLeaf a of
                Nothing -> getAttribute checkBranch checkLeaf =<< nodeParent tree
                Just val -> return val

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

