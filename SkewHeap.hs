module SkewHeap
( SkewHeap, insert, (<>), merge, getMin,
  extractMin, empty, size,
  fromList, toList, sort
) where

-- No need for comments - the code is self-documenting.
-- The basic operations such as insert, merge and extractMin have O(lgn)
-- amortized complexity. toList, fromList and sort are obviously O(nlgn).
data SkewHeap a = Empty | Node a (SkewHeap a) (SkewHeap a)

instance (Ord a) => Eq (SkewHeap a) where
    sh1 == sh2 = (toList sh1) == (toList sh2)

instance (Show a) => Show (SkewHeap a) where
    show Empty = "SkewHeap{}"
    show (Node val _ _) = "SkewHeap{" ++ show val ++ "..}"

insert :: Ord a => a -> SkewHeap a -> SkewHeap a
insert x sh = singleton x <> sh
  where singleton x = Node x Empty Empty

(<>) :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
(<>) = merge

merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge sh1 Empty = sh1
merge Empty sh2 = sh2
merge sh1@(Node v1 l1 r1) sh2@(Node v2 l2 r2)
    | v1 < v2   = Node v1 (sh2 <> r1) l1
    | otherwise = Node v2 (sh1 <> r2) l2

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
fromList = foldl (flip insert) Empty
-- fromList = foldr insert Empty

toList :: Ord a => SkewHeap a -> [a]
toList sh = case extractMin sh of Nothing -> []
                                  Just (val, sh1) -> val : toList sh1

sort :: Ord a => [a] -> [a]
sort = toList . fromList
