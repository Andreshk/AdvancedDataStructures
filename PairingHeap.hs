module PairingHeap
( insert, merge, (<>), getMin,
  extractMin, empty, size,
  fromList, toList, sort
) where

import Data.List (foldl')

-- No need for comments - the code is self-documenting.
-- The basic operations such as insert, merge and findMin have O(1)
-- amortized time complexity, and extractMin - O(lgn) amortized.
-- toList, fromList and sort are obviously O(nlgn).
data PairingHeap a = Empty | Node a [PairingHeap a]

instance (Ord a) => Eq (PairingHeap a) where
    Empty == Empty = True
    h1 == h2 = (toList h1) == (toList h2)

instance (Show a) => Show (PairingHeap a) where
    show Empty = "PairingHeap{}"
    show h = let Just val = getMin h in "PairingHeap{" ++ show val ++ "..}"
    
insert :: Ord a => a -> PairingHeap a -> PairingHeap a
insert x t = singleton x <> t
  where singleton x = Node x []

merge :: Ord a => PairingHeap a -> PairingHeap a -> PairingHeap a
merge ph1 Empty = ph1
merge Empty ph2 = ph2
merge ph1@(Node v1 ch1) ph2@(Node v2 ch2)
    | v1 < v2   = Node v1 (ph2:ch1)
    | otherwise = Node v2 (ph1:ch2)

(<>) :: Ord a => PairingHeap a -> PairingHeap a -> PairingHeap a
(<>) = merge

getMin :: PairingHeap a -> Maybe a
getMin Empty = Nothing
getMin (Node val _) = Just val

extractMin :: Ord a => PairingHeap a -> Maybe (a, PairingHeap a)
extractMin Empty = Nothing
extractMin (Node val children) = Just (val, makePairs children)
  where makePairs [] = Empty
        makePairs [x] = x
        makePairs (x:y:xs) = (x<>y) <> makePairs xs

empty :: PairingHeap a -> Bool
empty Empty = True
empty _ = False

-- unfortunately O(n)
size :: Ord a => PairingHeap a -> Int
size = length . toList

fromList :: Ord a => [a] -> PairingHeap a
fromList = foldl' (flip insert) Empty
-- fromList = foldr insert Empty

toList :: Ord a => PairingHeap a -> [a]
toList = toList' . extractMin
  where toList' Nothing = []
        toList' (Just (val, t)) = val : toList' (extractMin t)

sort :: Ord a => [a] -> [a]
sort = toList . fromList
