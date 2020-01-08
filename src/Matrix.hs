module Matrix where

import Data.Matrix

matrixSum :: (Num a) => Matrix a -> Matrix a -> Matrix a
matrixSum a b = matrix (nrows a) (ncols a) $ \(i, j) -> getElem i j a + getElem i j b

