module Lib
    (
    subfactorialRec,
    subfactorialList,
    ) where

subfactorialRec n = subfactorialRec' n n
    where subfactorialRec' n k 
            | k < 0 = 0
            | otherwise = minusOne k * factorialAdivFactorialB n k + subfactorialRec' n (k - 1)
          factorialAdivFactorialB n k
            | n == k = 1
            | otherwise = n * factorialAdivFactorialB (n - 1) k
          minusOne k = if even k then 1 else -1

subfactorialList n = sum [minusOne k * factorialAdivFactorialB n k | k <- [0 .. n]]
    where factorialAdivFactorialB n k
            | n == k = 1
            | otherwise = product [(k + 1) .. n]
          minusOne k = if even k then 1 else -1
