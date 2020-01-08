module Main where

import Prelude hiding (catch)
import Data.List (sortBy)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import System.Exit
import Control.Exception
import Control.DeepSeq

import Control.Parallel
import Control.Monad
import Text.Printf

import Data.Matrix

matrixSum :: (Num a) => Matrix a -> Matrix a -> Matrix a
matrixSum a b = matrix (nrows a) (ncols a) $ \(i, j) -> getElem i j a + getElem i j b

cutoff = 35

fib' :: Int -> Integer
fib' 0 = 0
fib' 1 = 1
fib' n = fib' (n-1) + fib' (n-2)

fib :: Int -> Integer
fib n | n < cutoff = fib' n
      | otherwise  = r `par` (l `pseq` l + r)
 where
    l = fib (n-1)
    r = fib (n-2)

main = forM_ [0..45] $ \i ->
            printf "n=%d => %d\n" i (fib i)
            