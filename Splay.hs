module Splay (empty,insert,lookup,fromList,toList) where
import Prelude hiding (lookup)

data Splay a = Empty | Node a (Splay a) (Splay a) deriving Show

-- Standard tree rotations
rotateLeft, rotateRight :: Splay a -> Splay a
rotateLeft  (Node x a (Node y b c)) = (Node y (Node x a b) c)
rotateRight (Node y (Node x a b) c) = (Node x a (Node y b c))

-- Naive implementation, does not splay optimally (!)
-- Invariant: `insert x _` returns a tree with x in its root
insert :: Ord a => a -> Splay a -> Splay a
insert x Empty = Node x Empty Empty
insert x t@(Node val l r)
  | x == val = t
  | x < val  = rotateRight (Node val (insert x l) r)
  | x > val  = rotateLeft  (Node val l (insert x r))

-- Naive implementation, does not splay optimally (!)
-- Invariant: if `lookup x _` returns a tree, its root is x
lookup :: Ord a => a -> Splay a -> Maybe (Splay a)
lookup _ Empty = Nothing
lookup x t@(Node val l r)
  | x == val = Just t
  | x < val  = (\l1 -> rotateRight (Node val l1 r)) <$> lookup x l
  | x > val  = (\r1 -> rotateLeft  (Node val l r1)) <$> lookup x r

fromList :: Ord a => [a] -> Splay a
fromList = foldr insert Empty

toList :: Splay a -> [a]
toList Empty = []
toList (Node val l r) = toList l ++ [val] ++ toList r

main = do
  putStrLn "> let t = fromList [1,4,2,5,3,4,2,6]"
  let t = fromList [1,4,2,5,3,4,2,6]
  putStrLn "> lookup 3 t"
  print $ lookup 3 t
  putStrLn "> lookup 7 t"
  print $ lookup 7 t
