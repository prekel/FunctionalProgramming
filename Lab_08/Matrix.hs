module Matrix where

import Control.Parallel
import Control.Parallel.Strategies
import Data.List
import Data.Matrix
import Data.Maybe

matrixSum :: (Num a) => Matrix a -> Matrix a -> Matrix a
matrixSum a b = matrix (nrows a) (ncols a) $ \(i, j) -> getElem i j a + getElem i j b

matrixDetParallel :: Matrix Int -> Int
matrixDetParallel m = sum [(-1) ^ (i - 1) * m ! (i, 1) * dets !! (i - 1) | i <- [1 .. nrows m]]
  where
    parallelMap f xs = map f xs `using` parList rdeepseq
    dets = parallelMap detLaplace minors
    minors = [minorMatrix i 1 m | i <- [1 .. nrows m]]
