module Matrix where

import Control.Parallel
import Control.Parallel.Strategies
import Data.List
import Data.Matrix

matrixSum :: (Num a) => Matrix a -> Matrix a -> Matrix a
matrixSum a b = matrix (nrows a) (ncols a) $ \(i, j) -> getElem i j a + getElem i j b

detLaplace1 :: Num a => Matrix a -> a
detLaplace1 m = sum1 [(-1) ^ (i - 1) * m ! (i, 1) * detLaplace (minorMatrix i 1 m) | i <- [1 .. nrows m]]
  where
    sum1 = foldl1' (+)

qwe1 f xs = map f xs `using` parList rdeepseq

f1 a = a * 2
xs1 = [1,2,3]

det m = sum [(-1) ^ (i - 1) * m ! (i, 1) * j | i <- [1 .. nrows m], j <- res1] / 3
  where
    res1 = qwe1 f2 xs2
    f2 = detLaplace
    xs2 = [minorMatrix i 1 m | i <- [1 .. nrows m]]
