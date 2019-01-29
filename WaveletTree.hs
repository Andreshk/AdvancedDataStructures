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
          where wavelet' p@(a,b) xs
                  | a == b    = Leaf a
                  | null ys   = Node nil Dummy right   {- Nodes with a single child will be skipped -}
                  | null zs   = Node nil left Dummy    {- during traversal => do not build a bitmap.-}
                  | otherwise = Node bitmap left right
                  where mid = midpoint a b
                        (ys,zs) = partition (<=mid) xs
                        bitmap = fromBits $ map (>mid) xs -- will not be evaluated if null ys || null zs
                        left = wavelet' (goL p) ys
                        right = wavelet' (goR p) zs

-- Rank: the number of set bits among the first i bits in the bitvector
rank :: Integral ix => ix -> BitVector -> ix
rank 0 bitmap = 0
rank i bitmap = BV.foldl (\c b -> if b then c+1 else c) 0 $ most i bitmap

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
wvrank c i (WaveletTree w p) = wvrank' c i w p
  where wvrank' _ i (Leaf _) _ = i -- all unused arguments should be the same symbol
        wvrank' c i (Node _ left Dummy) p  = wvrank' c i left (goL p)
        wvrank' c i (Node _ Dummy right) p = wvrank' c i right (goR p)
        wvrank' c i (Node bitmap left right) p@(a,b)
          | c <= (midpoint a b) = wvrank' c (i - rank i bitmap) left (goL p)
          | otherwise           = wvrank' c (rank i bitmap)     right (goR p)

test :: IO ()
test = do
  let str   = "abracadabra"
      (n,w) = (length str, wavelet str)
      str'  = (w!)<$>[0..n-1]
  print str'; print $ str==str' -- this check should always hold (!)
  mapM_ print (f w n <$> (sort.nub$str))
    where f w n c = map (\i->wvrank c i w) [1..n]
