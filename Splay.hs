import Prelude hiding (lookup)
data Splay = Empty | Node Int Splay Splay deriving Show

-- Important invariant: lookup returns an empty tree iff the argument is an empty tree
lookup :: Int -> Splay -> (Bool, Splay)
lookup _ Empty = (False, Empty)
lookup x t@(Node y l r)
  | x == y = (True, t)
  | x < y && empty l = (False, t)
  | x < y = let (b, l1) = lookup x l in (b, rotateRight (Node y l1 r)) -- [1]
  | x > y && empty r = (False, t)
  | x > y = let (b, r1) = lookup x r in (b, rotateLeft (Node y l r1))
  where empty Empty = True
        empty _     = False
        rotateLeft  (Node x a (Node y b c)) = (Node y (Node x a b) c)
        rotateRight (Node y (Node x a b) c) = (Node x a (Node y b c))
-- [1] l1 is always non-empty => handling b == False is not needed,
-- and will only save some tree rotations when the lookup is unsuccessful.
-- This may not be desired - in the current implementation if lookup v t
-- returns (False,t1), the root of t1 will be the value in t, closest to v.

test :: Splay
test = Node 5 (Node 4 (Node 2 (Node 1 Empty Empty)
                              (Node 3 Empty Empty))
                      Empty)
              (Node 6 Empty Empty)

main = do
  putStrLn "> test"
  print test
  putStrLn "> lookup 3 test"
  print $ lookup 3 test
  putStrLn "> lookup 7 test"
  print $ lookup 7 test
