{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module WaveletTree where
import Data.Char (chr,ord)
import Data.List (partition,sort,nub) -- sort,nub needed for testing only
import Data.BitVector (BitVector,BV,showBin,nil,(!.),most,fromBits) -- requires package bv
import qualified Data.BitVector as BV (foldl)

data Tree a = Dummy | Leaf a | Node BitVector (Tree a) (Tree a)
data WaveletTree a = WaveletTree (Tree a) (a,a)

class (Enum a, Ord a) => Wv a where
  midpoint :: a -> a -> a
instance Wv Char where
  midpoint x y = chr $ (ord x + ord y) `div` 2
instance {-# OVERLAPPABLE #-} (Integral a, Enum a, Ord a) => Wv a where
  midpoint x y = (x + y) `div` 2

goL, goR :: Wv a => (a,a) -> (a,a) -- placeholder names, pls rename
goL (a,b) = (a,midpoint a b)
goR (a,b) = (succ $ midpoint a b, b)

instance Show a => Show (WaveletTree a) where
  show (WaveletTree t _) = show' 0 t
    where show' pad Dummy    = replicate pad ' ' ++ "#"
          show' pad (Leaf x) = replicate pad ' ' ++ show x
          show' pad (Node bitmap left right) = replicate pad ' '
                                            ++ showBin bitmap
                                            ++ "\n" ++ show' (pad+2) left
                                            ++ "\n" ++ show' (pad+2) right

{- A sequence should support the following three operations:
    Access: the element at a given position in the sequence
    Rank: The number of occurences of a given element among the first i in the sequence
    Select: The position of the i-th occurence of a given element in the sequence -}
class Sequence c where
    type ElemType c :: *
    (!) :: c -> Int -> ElemType c
    rank, select :: Eq (ElemType c) => ElemType c -> Int -> c -> Int

-- Example implementation of the sequence operations for lists
instance Sequence [a] where
  type ElemType [a] = a
  (!) = (!!)
  rank c i lst = length . filter (==c) $ take i lst
  select c i (x:xs)
    | c == x    = if i == 1 then 0 else 1 + select c (i-1) xs
    | otherwise = 1 + select c i xs

instance Sequence BV where -- BitVector is actually a synonym
    type ElemType BV = Bool
    (!) = (!.)
    rank False i bv = i - (rank True i bv)
    rank True 0 _ = 0
    rank True i bitmap = BV.foldl (\c b -> if b then c+1 else c) 0 $ most i bitmap
    select b i bv = select' i 0
      where select' i curr
              | bv !. curr == b = if i == 1 then curr else select' (i-1) (curr+1)
              | otherwise       = select' i (curr+1)

wavelet :: Wv a => [a] -> WaveletTree a
wavelet xs = WaveletTree (wavelet' (a,b) xs) (a,b)
  where a = minimum xs
        b = maximum xs
        wavelet' range@(a,b) xs
          | a == b    = Leaf a
          | null ys   = Node nil Dummy right   {- Nodes with a single child will be skipped -}
          | null zs   = Node nil left Dummy    {- during traversal => do not build a bitmap.-}
          | otherwise = Node bitmap left right
          where mid = midpoint a b
                (ys,zs) = partition (<=mid) xs
                bitmap = fromBits $ map (>mid) xs -- will not be evaluated if null ys || null zs
                left  = wavelet' (goL range) ys
                right = wavelet' (goR range) zs

instance Wv a => Sequence (WaveletTree a) where
  type ElemType (WaveletTree a) = a
  (WaveletTree t _) ! i = t ! i
    where (Leaf x) ! _ = x
          (Node _ left Dummy) ! i = left ! i    {- A dummy node corresponds to an empty subset of -}
          (Node _ Dummy right) ! i = right ! i  {- characters => go directly to the other branch. -}
          (Node bitmap left right) ! i
            | bitmap !. i = right ! (rank True i bitmap)
            | otherwise   = left ! (rank False i bitmap)

  rank c i (WaveletTree w range) = rank' c i w range
    where rank' _ i (Leaf _) _ = i -- all unused arguments should contain the same symbol
          rank' c i (Node _ left Dummy) range  = rank' c i left (goL range)
          rank' c i (Node _ Dummy right) range = rank' c i right (goR range)
          rank' c i (Node bitmap left right) range@(a,b)
            | c <= (midpoint a b) = rank' c (rank False i bitmap) left (goL range)
            | otherwise           = rank' c (rank True i bitmap) right (goR range)

  select c i (WaveletTree w range) = select' c (i-1) w range
    where select' _ i (Leaf _) _ = i -- all unused arguments should contain the same symbol
          select' c i (Node _ left Dummy) range  = select' c i left (goL range)
          select' c i (Node _ Dummy right) range = select' c i right (goR range)
          select' c i (Node bitmap left right) range@(a,b)
            | c <= (midpoint a b) = let j = select' c i left (goL range)  in select False (j+1) bitmap
            | otherwise           = let j = select' c i right (goR range) in select True  (j+1) bitmap

test :: IO ()
test = do
  let str   = "abracadabra"
      (n,w) = (length str, wavelet str)
      str'  = (w!)<$>[0..n-1]
  print str'; print $ str==str' -- this check should always hold (!)
  mapM_ (print.(\c -> map (\i->rank c i w) [1..n])) (sort.nub$str)
  print $ let c = str!!0 in map (\i->select c i w) [1..length$filter(==c)str]
