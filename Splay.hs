module Splay (Splay,insert,lookup,fromList) where
import Prelude hiding (lookup)

data Splay a = Empty | Node a (Splay a) (Splay a)
-- Pretty-printing
instance Show a => Show (Splay a) where
  show t = "\n" ++ show' 0 t
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

-- The first pass: descend the tree in search of val, while recording how to
-- reconstruct it when going back upwards. Either creates a new node
-- if val is not found, or stops at the last node on the path to it.
find :: Ord a => Bool -> a -> (Splay a, [Crumb a]) -> (Splay a, [Crumb a])
find True val (Empty, cs) = (Node val Empty Empty, cs) -- val not found, add a new node
find False val p@(Node x Empty _, _) | val < x = p -- val not found, stop searching
find False val p@(Node x _ Empty, _) | val > x = p -- val not found, stop searching
find _ val p@(Node x _ _, _) | val == x = p -- found val somewhere in tree
find b val (Node x l r, cs)
  | val < x  = find b val (l, Crumb L x r : cs)
  | val > x  = find b val (r, Crumb R x l : cs)

-- The second, most crucial pass: given a subtree with root x and its path to
-- the root, reconstruct the entire tree using the breadcrumbs and simultaneously splay x up.
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

-- Inserting, the standard two-pass operation.
insert :: Ord a => a -> Splay a -> Splay a
insert val t = fst . splay $ find True val (t,[])

-- Looking up a value will splay either it if found,
-- or the last value on the search path if not found. 
lookup :: Ord a => a -> Splay a -> Splay a
lookup _ Empty = Empty
lookup val t = fst . splay $ find False val (t,[])

-- Split a tree into two by a given value
split :: Ord a => a -> Splay a -> (Splay a, Splay a)
split _ Empty = (Empty, Empty)
split val t
  | x <= val  = (Node x l Empty, r)
  | otherwise = (l, Node x Empty r)
  where (Node x l r) = lookup val t

-- Join two trees, given that all values in the first are less than the values in the second
join :: Ord a => Splay a -> Splay a -> Splay a
join t1 Empty = t1
join Empty t2 = t2
join t1 t2 = Node x l t2
  where (Node x l Empty) = fst . splay $ findMax (t1, [])
        findMax :: Ord a => (Splay a, [Crumb a]) -> (Splay a, [Crumb a])
        findMax p@(Node _ _ Empty, _) = p
        findMax (Node x l r, cs) = findMax (r, Crumb R x l : cs)

-- Removing a value, the splay tree way - look it up, splaying it
-- in the process, and on success join the two subtrees.
delete :: Ord a => a -> Splay a -> Splay a
delete _ Empty = Empty
delete val t
  | x == val  = join l r
  | otherwise = (Node x l r)
  where (Node x l r) = lookup val t

-- Build a tree from the set (!) of values in a list
fromList :: Ord a => [a] -> Splay a
fromList = foldl (flip insert) Empty
