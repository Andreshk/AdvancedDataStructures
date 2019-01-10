{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module WaveletTree where
import Data.Char (chr,ord)
import Data.List (partition)
import Data.BitVector (BitVector,showBin,nil,(!.),most,fromBits) -- requires package bv
import qualified Data.BitVector as BV (foldl)

data WaveletTree a = Dummy | Leaf a | Node BitVector (WaveletTree a) (WaveletTree a)

class (Enum a, Ord a) => Wv a where
  midpoint :: a -> a -> a
instance Wv Char where
  midpoint x y = chr $ (ord x + ord y) `div` 2
instance {-# OVERLAPPABLE #-} (Integral a, Enum a, Ord a) => Wv a where
  midpoint x y = (x + y) `div` 2

instance Show a => Show (WaveletTree a) where
  show w = show' 0 w
    where show' pad Dummy    = replicate pad ' ' ++ "#"
          show' pad (Leaf x) = replicate pad ' ' ++ show x
          show' pad (Node bitmap left right) = replicate pad ' '
                                            ++ showBin bitmap
                                            ++ "\n" ++ show' (pad+2) left
                                            ++ "\n" ++ show' (pad+2) right

wavelet :: Wv a => [a] -> WaveletTree a
wavelet xs = wavelet' (minimum xs) (maximum xs) xs
  where wavelet' from to xs
          | from == to = Leaf from
          | null ys    = Node nil Dummy right   {- Nodes with a single child will be skipped -}
          | null zs    = Node nil left Dummy    {- during indexing => do not build a bitmap. -}
          | otherwise  = Node bitmap left right
          where mid = midpoint from to
                (ys,zs) = partition (<=mid) xs
                bitmap = fromBits $ map (>mid) xs -- will not be evaluated if null ys || null zs
                left = wavelet' from mid ys
                right = wavelet' (succ mid) to zs

(!) :: Integral ix => WaveletTree a -> ix -> a
(Leaf x) ! _ = x
(Node _ left Dummy) ! i = left ! i    {- A dummy node corresponds to an empty subset of -}
(Node _ Dummy right) ! i = right ! i  {- characters => go directly to the other branch. -}
(Node bitmap left right) ! i
  | bitmap !. i = right ! (rank i)
  | otherwise   = left ! (i - rank i)
  where rank 0 = 0
        rank i = BV.foldl (\c b -> if b then c+1 else c) 0 $ most i bitmap

-- Should always return true (!)
check :: Wv a => [a] -> Bool
check xs = xs == map (wavelet xs !) [0..length xs - 1]
