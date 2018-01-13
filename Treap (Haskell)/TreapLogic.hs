module TreapLogic where

-- This module contains the actual treap logic.
-- Here the structure is named "Tree" in order to avoid
-- confusion. All specific treap functions are implemented here.
data Tree a = Empty | Node a Double (Tree a) (Tree a)

instance Show a => Show (Tree a) where
    show Empty = "Empty treap."
    show tr    = if size tr > 100
                 then "Treap too large for printing!"
                 else prettyPrint 0 tr

-- Pretty-printing a tree
prettyPrint :: Show a => Int -> Tree a -> String
prettyPrint _ Empty = ""
prettyPrint pad tr@(Node val pr left right) = 
    replicate pad ' ' ++ peekRoot tr ++ " -> " ++ peekRoot left ++ ", " ++ peekRoot right ++ "\n"
    ++ prettyPrint (pad+2) left
    ++ prettyPrint (pad+2) right
  where peekRoot :: Show a => Tree a -> String
        peekRoot Empty = "#"
        peekRoot (Node val pr _ _) = show val ++ " {" ++ show pr ++ "}"

-- Check whether a treap is empty
empty :: Tree a -> Bool
empty Empty = True
empty _ = False

-- Minimal and maximal value in a treap, via a helper function
getMin :: Tree a -> a
getMin (Node v _ Empty _) = v
getMin (Node _ _ left _)  = getMin left

getMax :: Tree a -> a
getMax (Node v _ _ Empty) = v
getMax (Node _ _ _ right) = getMax right

{-
Validity check:
- for each node we want all values in the left subtree to be smaller than the value
  in the node -> therefore TREE, as a binary search tree for the values
- for each node we want the priorities in its left and right children to be higher
  than the node priority -> therefore HEAP, formed by the node priorities
After each treap operation the treap can be checked for validity. Complexity: O(n)
-}
valid :: Ord a => Tree a -> Bool
valid Empty = True
valid t = valid' t (getMin t) (getMax t) 0.0
  where valid' :: Ord a => Tree a -> a -> a -> Double -> Bool
        valid' Empty _ _ _ = True
        valid' (Node val pr left right) min' max' pr'
            = pr >= pr' && min' <= val && val <= max'
            && valid' left  min' val pr
            && valid' right val max' pr

{-
Tree rabalancing is a series of rotation. During each insertion or deletion
only the nodes on the path from the root to the value in question need rotating,
meaning O(lgn) rotations on average. How a node should be rotated depends only
on the priorities in the two children nodes. The function either calls itself
recursively or is called during back-tracking of the insertion function.
This function also helps us rearrange the tree and either "sink" a node to
a leaf or "float" it to the root.
Such a rearrangement does not invalidate the BST invariant (!).
Complexity: O(lgn) average when the flag is set and the function calls itself
recursively to "sink" a value; and O(1) otherwise (local rebalance).
-}
rebalance :: Bool -> Tree a -> Tree a
rebalance flag tr@(Node _ _ Empty Empty) = if flag then Empty else tr
rebalance flag tr@(Node _ pr left right)
  | pr <= leftpr && pr <= rightpr = tr
  | leftpr < rightpr && flag      = let (Node v p l r) = rotateRight tr in (Node v p l (rebalance True r))
  | leftpr < rightpr              = rotateRight tr
  | leftpr >= rightpr && flag     = let (Node v p l r) = rotateLeft tr in (Node v p (rebalance True l) r)
  | leftpr >= rightpr             = rotateLeft tr
  where leftpr  = specialPr left
        rightpr = specialPr right
        specialPr :: Tree a -> Double
        specialPr Empty = 2.0
        specialPr (Node _ pr _ _) = pr

-- Left and right, or counter-clockwise and clockwise rotations
-- No other calls needed
-- Ever seen more elegant implementations? That's Haskell for you.
rotateLeft :: Tree a -> Tree a
rotateLeft (Node xv xp atr (Node yv yp btr ctr)) = Node yv yp (Node xv xp atr btr) ctr

rotateRight :: Tree a -> Tree a
rotateRight (Node yv yp (Node xv xp atr btr) ctr) = Node xv xp atr (Node yv yp btr ctr)

-- "Private" function for value insertion, used cleverly by split
-- and merge. Inserting a value works by adding it as in a regular
-- BST and rotating it up until the priorities form a valid heap.
-- Complexity: O(lgn) expected (and most often), O(n) worst-case
insert :: (Eq a, Ord a) => a -> Double -> Tree a -> Tree a
insert x newpr Empty = Node x newpr Empty Empty
insert x newpr tr@(Node val pr left right)
  | x < val  = rebalance False $ Node val pr (insert x newpr left) right
  | x > val  = rebalance False $ Node val pr left (insert x newpr right)
  | x == val = if newpr == (-1.0) -- used during split
               then rebalance False $ Node x newpr left right
               else tr

-- Value deletion: first we find the value, than increase its
-- priority and rebalance it downwards - with the new priority
-- it sinks and becomes a leaf, which is easily cut from the tree.
-- Complexity: O(lgn) expected (and most often), O(n) worst-case
remove :: (Eq a, Ord a) => a -> Tree a -> Tree a
remove _ Empty = Empty
remove x tr@(Node val pr left right)
    | x < val  = Node val pr (remove x left) right
    | x > val  = Node val pr left (remove x right)
    | x == val = rebalance True $ Node val 2.0 left right

-- Value search works like a standard BST
-- Complexity: O(lgn) expected (and most often), O(n) worst-case
search :: (Eq a, Ord a) => a -> Tree a -> Bool
search _ Empty = False
search x (Node val _ left right)
    | x < val  = search x left
    | x > val  = search x right
    | x == val = True

-- Merging two treaps requires the maximum value in the left treap
-- be smaller than the minimum value in the right treap. This is done
-- by adding a dummy value, which adopts the two treaps, and than sinking
-- this dummy value to a leaf and cutting it. For a dummy value we select
-- the root of one of the treaps - it is completely arbitrary.
-- Complexity: O(lgn) expected (and most often), O(n) worst-case
merge :: (Eq a, Ord a) => Tree a -> Tree a -> Maybe (Tree a)
merge Empty tr = Just tr
merge tr Empty = Just tr
merge tr1@(Node v _ _ _) tr2
    | leftmax >= rightmin = Nothing
    | otherwise           = Just $ rebalance True $ Node v 2.0 tr1 tr2 --(!)
  where leftmax  = getMax tr1
        rightmin = getMin tr2

-- Splitting a treap in two, such that one of them contains all values
-- smaller than a given value x, and the other one all the bigger values.
-- We insert x with priority, smaller than that of every other node,
-- so that after rebalancing it is located at the root. From the BST
-- invariant we then have our two resulting treaps as the two subtrees.
-- If x is contained before splitting, it will not be in any of the returned treaps.
-- Complexity: O(lgn) expected (and most often), O(n) worst-case
split :: (Eq a, Ord a) => a -> Tree a -> (Tree a, Tree a)
split x tr = let (Node _ _ left right) = insert x (-1.0) tr in (left, right)

-- Treap height
-- Complexity: O(n)
height :: Tree a -> Int
height Empty = 0
height (Node _ _ left right) = 1 + max (height left) (height right)

-- Value count
-- Complexity: O(n)
-- May run in O(1) if every treap carries the value count like its RNG,
-- but every insert/remove will need to return the new size along with the new tree.
size :: Tree a -> Int
size Empty = 0
size (Node _ _ left right) = 1 + (size left) + (size right)

-- All contained values in a sorted list.
-- "toList t1 ++ toList t2 == (toList $ merge t1 t2)"
-- holds true iff merging is allowed, whereas
-- "let (l,r) = split x t in toList l ++ toList r == toList t"
-- is true only when x was not present in t beforehand.
-- Complexity: O(n)
toList :: Tree a -> [a]
toList Empty = []
toList (Node val _ left right) = toList left ++ [val] ++ toList right