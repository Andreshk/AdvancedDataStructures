{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module WaveletTree (WaveletTree,wavelet,(!),rank,select,test) where
import Data.List (partition,sort,nub)
import Data.Function (on)
import Data.BitVector (BitVector,BV,(#),nil,fromBool,showBin,(!.),most,fromBits) -- requires package bv
import qualified Data.BitVector as BV (foldl)

{------ Huffman encoding section ------}
data HTree a = HLeaf a | (HTree a) :^: (HTree a)
data HPair a = HPair { tree :: HTree a, weight :: Int }
instance Eq  (HPair a) where (==) = (==) `on` weight
instance Ord (HPair a) where (<=) = (<=) `on` weight

huffman :: Eq a => [a] -> HTree a
huffman str = tree . head . mergeTrees $ histogram
  where histogram = sort [ HPair (HLeaf c) (length $ filter (==c) str) | c<-nub str ]
        mergeTrees [p] = [p]
        mergeTrees ((HPair t1 w1):(HPair t2 w2):ps) = mergeTrees $ insert (HPair (t1:^:t2) (w1+w2)) ps
          where insert p ps = let (a,b) = span (<p) ps in a++(p:b)

codes :: HTree a -> [(a,BitVector)]
codes (HLeaf c) = []
codes t = codes' t nil
  where codes' (HLeaf c) code = [(c,code)]
        codes' (t1:^:t2) code = codes' t1 (code # fromBool False) ++ codes' t2 (code # fromBool True)

codeOf :: Eq a => a -> [(a,BitVector)] -> BitVector
codeOf c = snd . head . filter ((==c).fst)

{------ Wavelet Tree ------}
data WTree a = Leaf a | Node BitVector (WTree a) (WTree a)
data WaveletTree a = WaveletTree (WTree a) [(a,BitVector)]

instance Show a => Show (WaveletTree a) where
  show (WaveletTree t _) = show' 0 t
    where show' pad (Leaf x) = replicate pad ' ' ++ show x
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

wavelet :: Eq a => [a] -> WaveletTree a
wavelet xs
  | null huffCodes = WaveletTree (Leaf $ xs!!0) []
  | otherwise      = WaveletTree (wavelet' huffTree 0 xs) huffCodes
  where huffTree = huffman xs
        huffCodes = codes huffTree
        wavelet' (HLeaf x) _ _= Leaf x
        wavelet' (hleft :^: hright) d xs = Node bitmap left right
          where (ys,zs) = partition (not.(!.d).(`codeOf` huffCodes)) xs
                bitmap = fromBits $ map ((!.d).(`codeOf` huffCodes)) xs
                left  = wavelet' hleft  (d+1) ys
                right = wavelet' hright (d+1) zs

instance Sequence (WaveletTree a) where
  type ElemType (WaveletTree a) = a
  (WaveletTree t _) ! i = t ! i
    where (Leaf x) ! _ = x
          (Node bitmap left right) ! i
            | bitmap !. i = right ! (rank True i bitmap)
            | otherwise   = left ! (rank False i bitmap)

  rank c i (WaveletTree w codes) = rank' i w 0
    where code = codeOf c codes
          rank' i (Leaf _) _ = i -- the leaf should now contain the symbol c
          rank' i (Node bitmap left right) d
            | code !. d = rank' (rank True i bitmap) right (d+1)
            | otherwise = rank' (rank False i bitmap) left (d+1)

  select c i (WaveletTree w codes) = select' (i-1) w 0
    where code = codeOf c codes
          select' i (Leaf _) _ = i -- the leaf should now contain the symbol c
          select' i (Node bitmap left right) d
            | code !. d = let j = select' i right (d+1) in select True  (j+1) bitmap
            | otherwise = let j = select' i left (d+1)  in select False (j+1) bitmap

test :: IO ()
test = do
  let str   = "abracadabra"
      (n,w) = (length str, wavelet str)
      str'  = (w!)<$>[0..n-1]
      chars = sort . nub $ str
  print str'; print $ str==str' -- this check should always hold (!)
  mapM_ (print.(\c -> map (\i->rank c i w) [1..n])) chars
  mapM_ (print.(\c -> map (\i->select c i w) [1..length$filter(==c)str])) chars
