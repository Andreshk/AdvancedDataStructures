import Prelude hiding (init)

data Tree a = L2 a | L3 a a | L4 a a a
            | N2 a (Tree a) (Tree a)
            | N3 a a (Tree a) (Tree a) (Tree a)
            | N4 a a a (Tree a) (Tree a) (Tree a) (Tree a)

-- Initialize a tree form a single value - there are no empty trees :)
init :: a -> Tree a
init = L2 

-- Pretty-printing designed to accent the same depth of the leaves
-- and show which values are in the same internal 3-node or 4-node.
instance Show a => Show (Tree a) where
    show t = "\n" ++ show' 0 t
      where show' pad (L2 x) = replicate pad ' ' ++ show x ++ "\n"
            show' pad (L3 x y) = concatMap (\v -> replicate pad ' ' ++ show v ++ "\n") [x,y]
            show' pad (L4 x y z) = concatMap (\v -> replicate pad ' ' ++ show v ++ "\n") [x,y,z]
            show' pad (N2 x a b) = show' (pad + newPad) a
                                ++ replicate pad ' ' ++ show x ++ " \n"
                                ++ show' (pad + newPad) b
              where newPad = succ . length $ show x -- add 1 to compensate for the explicit ' ' or '|' in the other cases
            show' pad (N3 x y a b c) = (unlines . map (\l -> replicate pad ' ' ++ " " ++ l) . lines $ show' newPad a)
                                    ++ replicate pad ' ' ++ show x ++ " \n"
                                    ++ (unlines . map (\l -> replicate pad ' ' ++ "|" ++ l) . lines $ show' newPad b)
                                    ++ replicate pad ' ' ++ show y ++ " \n"
                                    ++ (unlines . map (\l -> replicate pad ' ' ++ " " ++ l) . lines $ show' newPad c)
              where newPad = maximum $ map (length . show) [x,y]
            show' pad (N4 x y z a b c d) = (unlines . map (\l -> replicate pad ' ' ++ " " ++ l) . lines $ show' newPad a)
                                        ++ replicate pad ' ' ++ show x ++ " \n"
                                        ++ (unlines . map (\l -> replicate pad ' ' ++ "|" ++ l) . lines $ show' newPad b)
                                        ++ replicate pad ' ' ++ show y ++ " \n"
                                        ++ (unlines . map (\l -> replicate pad ' ' ++ "|" ++ l) . lines $ show' newPad c)
                                        ++ replicate pad ' ' ++ show z ++ " \n"
                                        ++ (unlines . map (\l -> replicate pad ' ' ++ " " ++ l) . lines $ show' newPad d)
              where newPad = maximum $ map (length . show) [x,y,z]

-- Searches for a value in a 2-3-4 tree
search :: Ord a => a -> Tree a -> Bool
search val (L2 x)     = val `elem` [x]
search val (L3 x y)   = val `elem` [x,y]
search val (L4 x y z) = val `elem` [x,y,z]
search val (N2 x a b)
  | val < x   = search val a
  | otherwise = val == x || search val b
search val (N3 x y a b c)
  | val < y   = search val (N2 x a b) -- lol
  | otherwise = val == y || search val c
search val (N4 x y z a b c d)
  | val < z   = search val (N3 x y a b c)
  | otherwise = val == z || search val d  

-- Inserts a value into a tree 
insert :: Ord a => a -> Tree a -> Tree a
insert val = fst . insert' val
  where -- Returns the new tree + whether the root has been split
        insert' :: Ord a => a -> Tree a -> (Tree a, Bool)
        insert' val (L2 x)
          | val < x   = (L3 val x, False)
          | otherwise = (L3 x val, False)
        insert' val (L3 x y)
          | val < x   = (L4 val x y, False)
          | val < y   = (L4 x val y, False)
          | otherwise = (L4 x y val, False)
        insert' val (L4 x y z) = (fst $ insert' val (N2 y (L2 x) (L2 z)), True) -- Split & try again
        insert' val (N2 x a b)
          | val < x   = case insert' val a of (N2 x' a' b', True) -> (N3 x' x a' b' b, False) -- Child was split, adopt middle value
                                              (t, False) -> (N2 x t b, False)
          | otherwise = case insert' val b of (N2 x' a' b', True) -> (N3 x x' a a' b', False) -- Child was split, adopt middle value
                                              (t, False) -> (N2 x a t, False)
        insert' val (N3 x y a b c)
          | val < x   = case insert' val a of (N2 x' a' b', True) -> (N4 x' x y a' b' b c, False) -- Child was split, adopt middle value
                                              (t, False) -> (N3 x y t b c, False)
          | val < y   = case insert' val b of (N2 x' a' b', True) -> (N4 x x' y a a' b' c, False) -- Child was split, adopt middle value
                                              (t, False) -> (N3 x y a t c, False)
          | otherwise = case insert' val c of (N2 x' a' b', True) -> (N4 x y x' a b a' b', False) -- Child was split, adopt middle value
                                              (t, False) -> (N3 x y a b t, False)
        insert' val (N4 x y z a b c d) = (fst $ insert' val (N2 y (N2 x a b) (N2 z c d)), True) -- Preemptively split & try again

-- Adds each value from a non-empty list into a tree,
-- in the given order (affecting the shape of the result).
fromList :: Ord a => [a] -> Tree a
fromList (x:xs) = foldl (flip insert) (init x) xs
