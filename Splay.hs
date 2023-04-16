module Splay (Splay,insert,lookup,fromList) where
import Prelude hiding (lookup)
import Data.Maybe (fromJust)

data Splay a = Empty | Node a (Splay a) (Splay a)
-- Pretty-printing
instance Show a => Show (Splay a) where
  show = show' 0
    where show' _ Empty = ""
          show' d (Node x l r) = show' (d+1) r ++ replicate (2*d) ' ' ++ show x ++ "\n" ++ show' (d+1) l

-- A list of directions encodes a path from the root to another node.
data Direction = L | R

-- A breadcrumb contains everything required to reconstruct a tree after a
-- step down - the direction we took, the root and the other subtree.
data Crumb a = Crumb Direction a (Splay a)

-- Standard tree rotations
rotate :: Direction -> Splay a -> Splay a
rotate L (Node x a (Node y b c)) = (Node y (Node x a b) c)
rotate R (Node y (Node x a b) c) = (Node x a (Node y b c))

-- The first pass: descend the tree in search of val, while recording how to reconstruct it when going
-- back upwards. Can either create a new node if val is not found, or return a Nothing.
find :: Ord a => Bool -> a -> (Splay a, [Crumb a]) -> Maybe (Splay a, [Crumb a])
find b val (Empty, cs) = if b then Just (Node val Empty Empty, cs) else Nothing
find b val p@(Node x l r, cs)
  | val == x = Just p -- found val somewhere in tree
  | val < x  = find b val (l, Crumb L x r : cs)
  | val > x  = find b val (r, Crumb R x l : cs)

-- The second, most crucial pass: given a subtree with root x and its path to
-- the root, reconstruct the entire tree and simultaneously splay x up.
splay :: (Splay a, [Crumb a]) -> (Splay a, [Crumb a])
splay (t, []) = (t, [])
-- Zig
splay (t, [Crumb L x r]) = (rotate R $ Node x t r, [])
splay (t, [Crumb R x l]) = (rotate L $ Node x l t, [])
-- Zig-zig
splay (t, (Crumb L x r : Crumb L x' r' : cs)) = splay (rotate R $ rotate R (Node x' (Node x t r) r'), cs)
splay (t, (Crumb R x l : Crumb R x' l' : cs)) = splay (rotate L $ rotate L (Node x' l' (Node x l t)), cs)
-- Zig-zag
splay (t, (Crumb L x r : Crumb R x' l : cs)) = splay (rotate L $ Node x' l (rotate R $ Node x t r), cs)
splay (t, (Crumb R x l : Crumb L x' r : cs)) = splay (rotate R $ Node x' (rotate L $ Node x l t) r, cs)

-- Inserting and looking up a value are practically identical two-pass operations.
insert :: Ord a => a -> Splay a -> Splay a
insert x t = fst . splay . fromJust $ find True x (t,[])

lookup :: Ord a => a -> Splay a -> Maybe (Splay a)
lookup x t = fst . splay <$> find False x (t, [])

-- Build a tree from the set (!) of values in a list
fromList :: Ord a => [a] -> Splay a
fromList = foldl (flip insert) Empty
