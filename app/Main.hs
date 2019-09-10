module Main where

import Lib

main :: IO ()
--main = someFunc
main = print "123"

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

a' :: (Num a) => [a] -> a
a' [] = 0
a' (x:xs) = x + sum' xs

linear a b 
    | a == 0 = []
    | otherwise = [-b / a]

quadratic a b c
    | a == 0 = []
    | det == 0 = [-b / (2 * a)]
    | det > 0  = [(-b - sqrt det) / (2 * a), (-b + sqrt det) / (2 * a)]
    | otherwise = []
    where det = b ^ 2 - 4 * a * c

checker a b c x = a * x * x + b * x + c

checker' x f 
    | f x == 0 = True
    | otherwise = False

--quadratic' a b c = [(-b - det) / (2 * a), (-b + det) / (2 * a), let det = b ^ 2 - 4 * a * c]