module WaveletTree where
import Data.List (partition)
import Data.BitVector (BitVector, showBin, (!.), most, fromBits) -- requires package bv
import qualified Data.BitVector as BV (foldl)

data WaveletTree a = Leaf a | Node BitVector (WaveletTree a) (WaveletTree a)

instance Show a => Show (WaveletTree a) where
  show w = show' 0 w
    where show' pad (Leaf x) = replicate pad ' ' ++ show x
          show' pad (Node bitmap left right) = replicate pad ' '
                                            ++ showBin bitmap
                                            ++ "\n" ++ show' (pad+2) left
                                            ++ "\n" ++ show' (pad+2) right

-- It is assumed that (minimum xs) == 1 and (maximum xs) is the size of the alphabet
wavelet :: Integral a => [a] -> WaveletTree a
wavelet xs = wavelet' (minimum xs) (maximum xs) xs
  where wavelet' from to xs
          | null xs    = Leaf (-1) -- fake node, will never be reached during indexing
          | from == to = Leaf from
          | otherwise  = Node bitmap (wavelet' from mid ys) (wavelet' (mid+1) to zs)
          where mid = (from + to) `div` 2
                bitmap = fromBits $ map (>mid) xs
                (ys,zs) = partition (<=mid) xs

(!) :: Integral ix => WaveletTree a -> ix -> a
(Leaf x) ! _ = x
(Node bitmap left right) ! i
  | bitmap !. i = right ! (rank i)
  | otherwise   = left ! (i - rank i)
  where rank 0 = 0
        rank i = BV.foldl (\c b -> if b then c+1 else c) 0 $ most i bitmap
