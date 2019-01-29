{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module WaveletTree where
import Data.Char (chr,ord)
import Data.List (partition,sort,nub) -- sort,nub needed for testing only
import Data.BitVector (BitVector,showBin,nil,(!.),most,fromBits) -- requires package bv
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

wavelet :: Wv a => [a] -> WaveletTree a
wavelet xs = WaveletTree t (a,b)
  where a = minimum xs
        b = maximum xs
        t = wavelet' (a,b) xs
          where wavelet' range@(a,b) xs
                  | a == b    = Leaf a
                  | null ys   = Node nil Dummy right   {- Nodes with a single child will be skipped -}
                  | null zs   = Node nil left Dummy    {- during traversal => do not build a bitmap.-}
                  | otherwise = Node bitmap left right
                  where mid = midpoint a b
                        (ys,zs) = partition (<=mid) xs
                        bitmap = fromBits $ map (>mid) xs -- will not be evaluated if null ys || null zs
                        left  = wavelet' (goL range) ys
                        right = wavelet' (goR range) zs

-- Rank: the number of set bits among the first i bits in the bitvector
rank :: Integral ix => ix -> BitVector -> ix
rank 0 bitmap = 0
rank i bitmap = BV.foldl (\c b -> if b then c+1 else c) 0 $ most i bitmap

-- Select: the position of the ith bit with the corresponding value
select :: Integral ix => Bool -> ix -> BitVector -> ix
select b i bv = select' i 0
  where select' i curr
          | bv !. curr == b = if i == 1 then curr else select' (i-1) (curr+1)
          | otherwise       = select' i (curr+1)

-- The i-th character from the original sequence
(!) :: Integral ix => WaveletTree a -> ix -> a
(WaveletTree t _) ! i = t ! i
  where (Leaf x) ! _ = x
        (Node _ left Dummy) ! i = left ! i    {- A dummy node corresponds to an empty subset of -}
        (Node _ Dummy right) ! i = right ! i  {- characters => go directly to the other branch. -}
        (Node bitmap left right) ! i
          | bitmap !. i = right ! (rank i bitmap)
          | otherwise   = left ! (i - rank i bitmap)

-- The number of occurences of a given symbol among the first i in the sequence
wvrank :: (Wv a, Integral ix) => a -> ix -> WaveletTree a -> ix
wvrank c i (WaveletTree w range) = wvrank' c i w range
  where wvrank' _ i (Leaf _) _ = i -- all unused arguments should contain the same symbol
        wvrank' c i (Node _ left Dummy) range  = wvrank' c i left (goL range)
        wvrank' c i (Node _ Dummy right) range = wvrank' c i right (goR range)
        wvrank' c i (Node bitmap left right) range@(a,b)
          | c <= (midpoint a b) = wvrank' c (i - rank i bitmap) left (goL range)
          | otherwise           = wvrank' c (rank i bitmap)     right (goR range)

-- The position of the i-th occurence of a given symbol in the sequence
wvselect :: (Wv a, Integral ix) => a -> ix -> WaveletTree a -> ix
wvselect c i (WaveletTree w range) = wvselect' c (i-1) w range
  where wvselect' _ i (Leaf _) _ = i -- all unused arguments should contain the same symbol
        wvselect' c i (Node _ left Dummy) range  = wvselect' c i left (goL range)
        wvselect' c i (Node _ Dummy right) range = wvselect' c i right (goR range)
        wvselect' c i (Node bitmap left right) range@(a,b)
          | c <= (midpoint a b) = let j = wvselect' c i left (goL range)  in select False (j+1) bitmap
          | otherwise           = let j = wvselect' c i right (goR range) in select True  (j+1) bitmap

test :: IO ()
test = do
  let str   = "abracadabra"
      (n,w) = (length str, wavelet str)
      str'  = (w!)<$>[0..n-1]
  print str'; print $ str==str' -- this check should always hold (!)
  mapM_ print (map (\c -> map (\i->wvrank c i w) [1..n]) (sort.nub$str))
  print $ let c = str!!0 in map (\i->wvselect c i w) [1..length$filter(==c)str]
