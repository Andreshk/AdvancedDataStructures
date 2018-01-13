module Treap
( makeTreap, empty, valid,
  insert, remove, search, merge, split,
  insertList, removeList, height, size,
  fromList, toList, sort
) where

import qualified TreapLogic as TL hiding (Node, prettyprint, rebalance, getExtreme, rotateLeft, rotateRight)
import System.Random (StdGen, mkStdGen, randomR)

{-
In order to preserve the principle of pure functional programming,
every Treap contains its random number generator. Every time a number is generated,
the new state of the generator is also returned. This means that every time we
create a Treap, whether with makeTreap or fromList, we need to pass the seed
for the RNG. This may seem in conflict with the idea of a randomized structure,
but it guarantees that every time we construct a treap with the same values and
seed, we will get the same well-balanced tree
-}

-- This structure acts as a wrapper to the real treap and delegates most of the
-- functionality. For more detailed description see TreapLogic.hs, where the
-- treap itself is implemented.
data Treap a = Treap (TL.Tree a) StdGen

instance Show a => Show (Treap a) where
    show (Treap tr _) = show tr

-- Constructing an empty treap from an RNG seed
makeTreap :: Int -> Treap a
makeTreap seed = Treap TL.Empty (mkStdGen seed)

-- Check whether a treap is empty
empty :: Treap a -> Bool
empty (Treap tr _) = TL.empty tr

-- Validity check
valid :: (Eq a, Ord a) => Treap a -> Bool
valid (Treap tr _) = TL.valid tr

-- Value insertion
insert :: (Eq a, Ord a) => a -> Treap a -> Treap a
insert x (Treap tr gen) = Treap newtr newgen
  where (newpr, newgen) = randomR (0.0, 1.0) gen
        newtr = TL.insert x newpr tr

-- Value deletion
remove :: (Eq a, Ord a) => a -> Treap a -> Treap a
remove x (Treap tr gen) = Treap (TL.remove x tr) gen

-- Value search
search :: (Eq a, Ord a) => a -> Treap a -> Bool
search x (Treap tr _) = TL.search x tr

-- Merging of two treaps
merge :: (Eq a, Ord a) => Treap a -> Treap a -> Maybe (Treap a)
merge (Treap tr1 gen) (Treap tr2 _)
  = case TL.merge tr1 tr2 of Nothing -> Nothing
                             Just tr -> Just $ Treap tr gen

-- Splitting a treap in two, such that one of them contains all values
-- lesser than s given value x and the other all bigger values.
split :: (Eq a, Ord a) => a -> Treap a -> (Treap a, Treap a)
split x (Treap tr gen) = (Treap left gen, Treap right gen)
  where (left, right) = TL.split x tr

-- Multiple value insertion
insertList :: (Eq a, Ord a) => [a] -> Treap a -> Treap a
insertList = flip $ foldl (flip insert)

-- Multiple value deletion
removeList :: (Eq a, Ord a) => [a] -> Treap a -> Treap a
removeList = flip $ foldl (flip remove)

-- Treap height
height :: Treap a -> Int
height (Treap tr _) = TL.height tr

-- Contained value count
size :: Treap a -> Int
size (Treap tr _) = TL.size tr

-- Constructing a treap from a list of numbers and seed
-- complexity: O(nlgn)
fromList :: (Eq a, Ord a) => Int -> [a] -> Treap a
fromList seed lst = insertList lst (makeTreap seed)

-- Sorted list from all the values in a treap
toList :: Treap a -> [a]
toList (Treap tr _) = TL.toList tr

-- Sorting a list by inserting all values in a treap and
-- extracting them in order. Obviously O(nlgn) expected complexity.
-- The seed needs to be hardcoded.
sort :: (Eq a, Ord a) => [a] -> [a]
sort = toList . fromList 20120618

--iei
