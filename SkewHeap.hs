module SkewHeap
( insert, merge, (<>), getMin,
  extractMin, empty, size,
  fromList, toList, sort
) where

import Data.List (foldl')

-- No need for comments - the code is self-documenting.
-- The basic operations such as insert, merge and extractMin have O(lgn)
-- amortized complexity. toList, fromList and sort are obviously O(nlgn).
data SkewHeap a = Empty | Node a (SkewHeap a) (SkewHeap a)

instance (Ord a) => Eq (SkewHeap a) where
    Empty == Empty = True
    h1 == h2 = (toList h1) == (toList h2)

instance (Show a) => Show (SkewHeap a) where
    show Empty = "SkewHeap{}"
    show h = let Just val = getMin h in "SkewHeap{" ++ show val ++ "..}"

insert :: Ord a => a -> SkewHeap a -> SkewHeap a
insert x t = singleton x <> t
  where singleton x = Node x Empty Empty

merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge t1 Empty = t1
merge Empty t2 = t2
merge t1@(Node v1 l1 r1) t2@(Node v2 l2 r2)
    | v1 < v2   = Node v1 (t2 <> r1) l1
    | otherwise = Node v2 (t1 <> r2) l2

(<>) :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
(<>) = merge

getMin :: SkewHeap a -> Maybe a
getMin Empty = Nothing
getMin (Node val _ _) = Just val

extractMin :: Ord a => SkewHeap a -> Maybe (a, SkewHeap a)
extractMin Empty = Nothing
extractMin (Node val left right) = Just (val, left <> right)

empty :: SkewHeap a -> Bool
empty Empty = True
empty _ = False

-- unfortunately O(n)
size :: Ord a => SkewHeap a -> Int
size = length . toList

fromList :: Ord a => [a] -> SkewHeap a
fromList = foldl' (flip insert) Empty
-- fromList = foldr insert Empty

toList :: Ord a => SkewHeap a -> [a]
toList = toList' . extractMin
  where toList' Nothing = []
        toList' (Just (val, t)) = val : toList' (extractMin t)

sort :: Ord a => [a] -> [a]
sort = toList . fromList