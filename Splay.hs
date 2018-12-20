import Prelude hiding (lookup)
data Splay = Empty | Node Int Splay Splay deriving Show

-- Naive implementation, does not splay optimally
lookup :: Int -> Splay -> Maybe Splay
lookup _ Empty = Nothing
lookup x t@(Node y l r)
  | x == y = Just t
  | x < y && empty l = Nothing
  | x < y = (\l1 -> rotateRight (Node y l1 r)) <$> lookup x l
  | x > y && empty r = Nothing
  | x > y = (\r1 -> rotateLeft  (Node y l r1)) <$> lookup x r
  where empty Empty = True
        empty _     = False
        rotateLeft  (Node x a (Node y b c)) = (Node y (Node x a b) c)
        rotateRight (Node y (Node x a b) c) = (Node x a (Node y b c))

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
