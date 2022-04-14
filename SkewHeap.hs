module SkewHeap
( SkewHeap, insert, (<>), merge, getMin,
  extractMin, empty, size,
  fromList, toList, sort
) where
import Prelude hiding ((<>))
import Data.List (unfoldr)

-- No need for comments - the code is self-documenting.
-- The basic operations such as insert, merge and extractMin have O(lgn)
-- amortized complexity. toList, fromList and sort are obviously O(nlgn).
data SkewHeap a = Empty | Node a (SkewHeap a) (SkewHeap a)

-- The characteristic operation - a skew merge
(<>) :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
sh1 <> Empty = sh1
Empty <> sh2 = sh2
sh1@(Node v1 l1 r1) <> sh2@(Node v2 l2 r2)
    | v1 < v2   = Node v1 (sh2 <> r1) l1
    | otherwise = Node v2 (sh1 <> r2) l2

-- All other operations are then reduced to a skew merge
insert :: Ord a => a -> SkewHeap a -> SkewHeap a
insert x sh = singleton x <> sh
  where singleton x = Node x Empty Empty

merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge = (<>)

extractMin :: Ord a => SkewHeap a -> Maybe (a, SkewHeap a)
extractMin Empty = Nothing
extractMin (Node val left right) = Just (val, left <> right)

-- Other basic operations
getMin :: SkewHeap a -> Maybe a
getMin Empty = Nothing
getMin (Node val _ _) = Just val

empty :: SkewHeap a -> Bool
empty Empty = True
empty _ = False

-- Unfortunately O(n)
size :: Ord a => SkewHeap a -> Int
size = length . toList

-- The beauty of folding & unfolding
fromList :: Ord a => [a] -> SkewHeap a
fromList = foldr insert Empty

toList :: Ord a => SkewHeap a -> [a]
toList = unfoldr extractMin

sort :: Ord a => [a] -> [a]
sort = toList . fromList
