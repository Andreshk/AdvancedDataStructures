module CartesianTree where

data CartesianTree a = Empty | Node a (CartesianTree a) (CartesianTree a)

-- Used purely for debugging purposes
instance Show a => Show (CartesianTree a) where
    show t = let (res, _, _) = show' t in init $ unlines res
      where -- Returns the lines of the visualized tree + dimension of the rectangle it fits in.
            -- See below for its postconditions (checked at every return).
            show' :: Show a => CartesianTree a -> ([String], Int, Int)
            show' Empty = checkPost ([],0,0)
            show' (Node val left right) = checkPost (first:rest, lw+vw+rw, rows+1)
              where pad :: Int -> String
                    pad n = replicate n ' '
                    first = (pad lw) ++ show val ++ (pad rw)
                    vw = length $ show val
                    (lres, lw, lh) = show' left
                    (rres, rw, rh) = show' right
                    rows = max lh rh
                    rest = zipWith (\l r -> l ++ (pad vw) ++ r) (lres ++ replicate (rows - lh) (pad lw))
                                                                (rres ++ replicate (rows - rh) (pad rw))
            -- Checks the show' postconditions (impurely). Can be overridden to id.
            checkPost (res,w,h) = if ok then (res,w,h) else error "show' postcondition violated"
              where ok = length res == h && all ((==w).length) res

-- Omega(nlgn), but O(n^2) if the tree ends up imbalanced (no control over that)
cartesian :: Ord a => [a] -> CartesianTree a
cartesian []  = Empty
cartesian [x] = Node x Empty Empty -- unneeded, really
cartesian lst = Node m (cartesian lst1) (cartesian lst2)
  where m = minimum lst
        (lst1,(_:lst2)) = span (>m) lst

-- Also Omega(nlgn) and O(n^2), but expected to be faster in practice.
-- The (<<) operator inserts a value down the right spine of a tree,
-- therefore building the tree is calling (<<) for each value in the list.
cartesian' :: Ord a => [a] -> CartesianTree a
cartesian' = foldl (<<) Empty
  where (<<) :: Ord a => CartesianTree a -> a -> CartesianTree a
        Empty                    << x = Node x Empty Empty
        tr@(Node val left right) << x
          | x < val   = Node x tr Empty
          | otherwise = Node val left (right << x)

-- Reduction from LCA to 0-1 RMQ (pardon the ugly function name)
lca2rmq01 :: CartesianTree a -> [Int]
lca2rmq01 Empty = []
lca2rmq01 tr    = helper tr 0
  where helper (Node _ left right) d = [d] ++ try left d ++ try right d
        -- Output the root depth, followed by the values for the left
        -- subtree (if any) and those for the right subtree (if any).
        -- If there are any values in any of the two subtrees, we should output the root again.
        -- Invariant: helper (unlike try) is always called on non-empty trees.
        try Empty _ = []
        try tr    d = helper tr (d+1) ++ [d]
