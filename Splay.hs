module Splay (insert,lookup,fromList,toList) where
import Prelude hiding (lookup)

data Splay a = Empty | Node a (Splay a) (Splay a) deriving Show

-- Standard tree rotations
rotateLeft, rotateRight :: Splay a -> Splay a
rotateLeft  (Node x a (Node y b c)) = (Node y (Node x a b) c)
rotateRight (Node y (Node x a b) c) = (Node x a (Node y b c))

-- A list of directions forms a path from the root to another node.
data Direction = L | R deriving Eq

-- Two-pass, bottom-up approach: insert the new value, returning
-- both the new tree and the path to the inserted value, and then
-- move this value to the root using the path.
insert :: Ord a => a -> Splay a -> Splay a
insert x t = let (path, t') = pathToInserted x t in splay t' path
  where pathToInserted :: Ord a => a -> Splay a -> ([Direction], Splay a)
        pathToInserted x Empty = ([], Node x Empty Empty)
        pathToInserted x t@(Node val l r)
          | x == val = ([], t) -- x is already present, do not add it again
          | x < val  = let (path,l') = pathToInserted x l in (L:path, Node val l' r)
          | x > val  = let (path,r') = pathToInserted x r in (R:path, Node val l r')

-- Two-pass, bottom-up approach: first find a path to the searched
-- value, and if found, use this path to splay it to the root.
lookup :: Ord a => a -> Splay a -> Maybe (Splay a)
lookup x t = splay t <$> pathTo x t
  where pathTo :: Ord a => a -> Splay a -> Maybe [Direction]
        pathTo _ Empty = Nothing
        pathTo x (Node val l r)
          | x == val = Just []
          | x < val  = (L:) <$> pathTo x l
          | x > val  = (R:) <$> pathTo x r

-- The most crucial function: given a path to a value (either from lookup or
-- as a result from insertion), move it to the root, while preserving tree
-- balance & keeping recently accessed values still close to the new root.
splay :: Splay a -> [Direction] -> Splay a
splay t [] = t -- The value is already at the root
-- Zig
splay t [L] = rotateRight t
splay t [R] = rotateLeft  t
-- Zig-zig
splay (Node q (Node p x c) d) (L:L:path) = rotateRight $ rotateRight (Node q (Node p (splay x path) c) d)
splay (Node p a (Node q b x)) (R:R:path) = rotateLeft  $ rotateLeft  (Node p a (Node q b (splay x path)))
-- Zig-zag
splay (Node q (Node p a x) d) (L:R:path) = rotateRight (Node q (rotateLeft  $ Node p a (splay x path)) d)
splay (Node p a (Node q x d)) (R:L:path) = rotateLeft  (Node p a (rotateRight $ Node q (splay x path) d))

-- Build a tree from the set (!) of values in a list
fromList :: Ord a => [a] -> Splay a
fromList = foldr insert Empty

-- Make a sorted list from all the contained elements
toList :: Splay a -> [a]
toList Empty = []
toList (Node val l r) = toList l ++ [val] ++ toList r

-- Naive implementation, does not splay optimally (!)
-- Invariant: `insert' x _` returns a tree with x in its root
insert' :: Ord a => a -> Splay a -> Splay a
insert' x Empty = Node x Empty Empty
insert' x t@(Node val l r)
  | x == val = t
  | x < val  = rotateRight (Node val (insert' x l) r)
  | x > val  = rotateLeft  (Node val l (insert' x r))

-- Naive implementation, does not splay optimally (!)
-- Invariant: if `lookup x _` returns a tree, its root is x
lookup' :: Ord a => a -> Splay a -> Maybe (Splay a)
lookup' _ Empty = Nothing
lookup' x t@(Node val l r)
  | x == val = Just t
  | x < val  = (\l1 -> rotateRight (Node val l1 r)) <$> lookup' x l
  | x > val  = (\r1 -> rotateLeft  (Node val l r1)) <$> lookup' x r

-- Demo:
main = do
  putStrLn "> let t = fromList [1,4,2,5,3,4,2,6]"
  let t = fromList [1,4,2,5,3,4,2,6]
  putStrLn "> lookup 3 t"
  print $ lookup 3 t
  putStrLn "> lookup 7 t"
  print $ lookup 7 t
