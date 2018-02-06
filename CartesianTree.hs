module CartesianTree where

data CartesianTree = Empty | Node Int CartesianTree CartesianTree
  deriving (Eq, Show) -- both used for debugging, neither required for normal work

-- Omega(nlgn), but O(n^2) if the tree ends up imbalanced (no control over that)
cartesian :: [Int] -> CartesianTree
cartesian []  = Empty
cartesian [x] = Node x Empty Empty -- unneeded, really
cartesian lst = Node m (cartesian lst1) (cartesian lst2)
  where m = minimum lst
        (lst1,(_:lst2)) = span (>m) lst

-- Also Omega(nlgn) and O(n^2), but expected to be faster in practice.
-- The (<<) operator inserts a value down the right spine of a tree,
-- therefore building the tree is calling (<<) for each value in the list.
cartesian' :: [Int] -> CartesianTree
cartesian' = foldl (<<) Empty
  where (<<) :: CartesianTree -> Int -> CartesianTree
        Empty                    << x = Node x Empty Empty
        tr@(Node val left right) << x
          | x < val   = Node x tr Empty
          | otherwise = Node val left (right << x)

-- Reduction from LCA to 0-1 RMQ (pardon the ugly function name)
lca2rmq01 :: CartesianTree -> [Int]
lca2rmq01 Empty = []
lca2rmq01 tr    = helper tr 0
  where helper (Node _ left right) d = [d] ++ try left d ++ try right d
        -- Output the root depth, followed by the values for the left
        -- subtree (if any) and those for the right subtree (if any).
        -- If there are any values in any of the two subtrees, we should output the root again.
        -- Invariant: helper (unlike try) is always called on non-empty trees.
        try Empty _ = []
        try tr    d = helper tr (d+1) ++ [d]
