module Treap (mkTreap, empty, valid, insert, remove, search,
  merge, split, height, size, fromList, toList, sort, sortIO) where
import System.Random (StdGen, mkStdGen, random, newStdGen)
import qualified System.Random as Random (split)

-- In order to keep the Treap, a randomized data structure, pure, every Treap
-- carries with itself a random number generator, provided on Treap creation.
-- All complexities are O(lgn) expected, O(n) worst-case except when noted otherwise.

-- The underlying tree structure
data Tree a = Empty | Node a Double (Tree a) (Tree a)

-- This structure acts as a wrapper to the real tree
-- and delegates most of the functionality to it.
data Treap a = Treap (Tree a) StdGen

instance Show a => Show (Treap a) where
    show (Treap Empty _) = "Empty treap."
    show (Treap t _) = show' 0 t
      where show' :: Show a => Int -> Tree a -> String
            show' _ Empty = ""
            show' pad t@(Node val pr left right) = 
                replicate pad ' ' ++ peekRoot t ++ " -> " ++ peekRoot left ++ ", " ++ peekRoot right ++ "\n"
                ++ show' (pad+2) left
                ++ show' (pad+2) right
            peekRoot :: Show a => Tree a -> String
            peekRoot Empty = "#"
            peekRoot (Node val pr _ _) = show val ++ " {" ++ show pr ++ "}"

-- Constructing an empty treap from an RNG seed
mkTreap :: Int -> Treap a
mkTreap seed = Treap Empty (mkStdGen seed)

-- Check whether a treap is empty
empty :: Treap a -> Bool
empty (Treap Empty _) = True
empty _ = False

-- Minimum and maximum value in a tree
getMin, getMax :: Tree a -> a
getMin (Node v _ Empty _) = v
getMin (Node _ _ left _)  = getMin left
getMax (Node v _ _ Empty) = v
getMax (Node _ _ _ right) = getMax right

-- Treap validity check:
-- - the node values should form a valid binary search tree
-- - the node priorities should form a valid min-heap
valid :: (Eq a, Ord a) => Treap a -> Bool
valid (Treap t _) = valid' t (getMin t) (getMax t) 0.0
  where valid' :: (Eq a, Ord a) => Tree a -> a -> a -> Double -> Bool
        valid' Empty _ _ _ = True
        valid' (Node val pr left right) min' max' pr'
            = pr >= pr' && min' <= val && val <= max'
            && valid' left  min' val pr
            && valid' right val max' pr


-- Left and right, or counter-clockwise and clockwise tree rotations.
-- Note: does not violate BST invariant.
rotateLeft, rotateRight :: Tree a -> Tree a
rotateLeft  (Node xv xp a (Node yv yp b c)) = Node yv yp (Node xv xp a b) c
rotateRight (Node yv yp (Node xv xp a b) c) = Node xv xp a (Node yv yp b c)

-- When a value is inserted/deleted, it is done so by the BST property.
-- Afterwards the tree is rebalanced via a series of rotations on the path
-- to this value in order to restore the heap property of the priorities.
-- This function either calls itself recursively during deletion (to "sink" a value)
-- or is called during the back-tracking after insertion (to "float" a value).
-- Complexity: O(lgn) expected for a recursive call, O(1) otherwise (local rebalance)
rebalance :: Bool -> Tree a -> Tree a
rebalance recurse t@(Node _ _ Empty Empty) = if recurse then Empty {- cut this leaf -} else t
rebalance recurse t@(Node _ pr left right)
  | pr <= leftpr && pr <= rightpr = t -- Heap property restored
  | leftpr < rightpr && recurse   = let (Node v p l r) = rotateRight t in (Node v p l (rebalance True r))
  | leftpr < rightpr              = rotateRight t
  | leftpr >= rightpr && recurse  = let (Node v p l r) = rotateLeft t in (Node v p (rebalance True l) r)
  | leftpr >= rightpr             = rotateLeft t
  where leftpr  = specialPr left
        rightpr = specialPr right
        specialPr :: Tree a -> Double
        specialPr Empty = 2.0
        specialPr (Node _ pr _ _) = pr

-- Helper function for insertion, also used cleverly by split & merge.
insert' :: (Eq a, Ord a) => a -> Double -> Tree a -> Tree a
insert' x newpr Empty = Node x newpr Empty Empty
insert' x newpr t@(Node val pr left right)
  | x < val         = rebalance False $ Node val pr (insert' x newpr left) right
  | x > val         = rebalance False $ Node val pr left (insert' x newpr right)
  | newpr == (-1.0) = rebalance False $ Node x newpr left right
  | otherwise       = t

-- Value insertion - add as in a BST, but recover the heap property on the way back
insert :: (Eq a, Ord a) => a -> Treap a -> Treap a
insert x (Treap t gen) = Treap (insert' x newpr t) newgen
  where (newpr, newgen) = random gen

-- Value deletion - use a "heavy" priority value to move the value to a leaf & just cut that leaf
remove :: (Eq a, Ord a) => a -> Treap a -> Treap a
remove x (Treap t gen) = Treap (remove' x t) gen
  where remove' :: (Eq a, Ord a) => a -> Tree a -> Tree a
        remove' _ Empty = Empty
        remove' x (Node val pr left right)
            | x < val   = Node val pr (remove' x left) right
            | x > val   = Node val pr left (remove' x right)
            | otherwise = rebalance True $ Node undefined 2.0 left right

-- Value search works exactly like in a standard BST
search :: (Eq a, Ord a) => a -> Treap a -> Bool
search x (Treap t _) = search' x t
  where search' :: (Eq a, Ord a) => a -> Tree a -> Bool
        search' _ Empty = False
        search' x (Node val _ left right)
            | x < val  = search' x left
            | x > val  = search' x right
            | x == val = True

-- Merging of two treaps requires the maximum value in the left treap
-- be smaller than the minimum value in the right treap. This is done
-- by adding a dummy value, which adopts the two treaps, and sinking
-- this dummy value to a leaf and cutting it.
merge :: (Eq a, Ord a) => Treap a -> Treap a -> Maybe (Treap a)
merge (Treap Empty _) _ = Nothing
merge _ (Treap Empty _) = Nothing
merge (Treap t1 gen) (Treap t2 _)
  | (getMax t1) >= (getMin t2) = Nothing
  | otherwise = Just (Treap (rebalance True $ Node undefined 2.0 t1 t2) gen) -- (!)

-- Splitting a treap in two, such that one of them contains all
-- values less than a given value x and the other all bigger values.
-- Note: x will not be present in any of the two resulting trees.
split :: (Eq a, Ord a) => a -> Treap a -> (Treap a, Treap a)
split x (Treap t gen) = (Treap left gen1, Treap right gen2)
  where (Node _ _ left right) = insert' x (-1.0) t
        (gen1, gen2) = Random.split gen

-- Treap height, unfortunately O(n)
height :: Treap a -> Int
height (Treap t _) = height' t
  where height' :: Tree a -> Int
        height' Empty = 0
        height' (Node _ _ left right) = 1 + max (height' left) (height' right)

-- Contained value count, also in O(n)
size :: Treap a -> Int
size (Treap t _) = size' t
  where size' :: Tree a -> Int
        size' Empty = 0
        size' (Node _ _ left right) = 1 + (size' left) + (size' right)

-- Constructing a treap from a list of numbers and seed in O(nlgn)
fromList :: (Eq a, Ord a) => StdGen -> [a] -> Treap a
fromList gen = foldr insert (Treap Empty gen)

-- Sorted list from all the values in a treap.
-- "toList t1 ++ toList t2 == (toList $ merge t1 t2)"
-- holds true iff merging is allowed, whereas
-- "let (l,r) = split x t in toList l ++ toList r == toList t"
-- is true only when x was not present in t beforehand.
toList :: Treap a -> [a]
toList (Treap t _) = toList' t
  where toList' :: Tree a -> [a]
        toList' Empty = []
        toList' (Node val _ left right) = toList' left ++ (val : toList' right)

-- Sorting a list by inserting all values in a treap (constructed w/ the given random
-- generator) and extracting them in order. Obviously O(nlgn) expected complexity.
sort :: (Eq a, Ord a) => StdGen -> [a] -> [a]
sort gen = toList . fromList gen

-- Convenience that runs in the IO monad & uses the global random generator.
sortIO :: (Eq a, Ord a) => [a] -> IO [a]
sortIO lst = do
  gen <- newStdGen
  return $ sort gen lst

-- to-do: an invariant-checking function that can be called
-- after insert'/remove' and lots, lots of tests...
