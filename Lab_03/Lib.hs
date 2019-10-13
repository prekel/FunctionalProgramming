module Lib
    (
    multifactorialRec,
    multifactorialList,
    factorialRec,
    factorialList,
    ) where

---- | Вариант 4

multifactorialRec n k
    | 0 < n && n <= k = n
    | n > k = n * (multifactorialRec (n - k) k)
    | otherwise = error "Нельзя неположительные"

multifactorialList n k
    | n <= 0 || k <= 0 = error "Нельзя неположительные"
    | otherwise = product [n, n - k..1]

factorialRec n
    | n == 0 = 1
    | n > 0 = n * factorialRec(n - 1)
    | otherwise = error "Нельзя отрицательные"
    
factorialList n
    | n < 0 = error "Нельзя отрицательные"
    | otherwise = product [1..n]
    