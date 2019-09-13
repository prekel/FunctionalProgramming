module Main where

import Lib

main :: IO ()
--main = someFunc
main = do
    line <- getLine
    let a = read line :: Double
    line <- getLine
    let b = read line :: Double
    line <- getLine
    let c = read line :: Double
    print (quadratic a b c)

linear a b
    | a == 0 && b == 0 = error "Infinite number of roots"
    | a == 0 && b /= 0 = []
    | b == 0 = [0]
    | otherwise = [-b / a]

discriminant a b c = b ^ 2 - 4 * a * c

quadratic a b c
    | a == 0 = linear b c
--    | a == 0 = error "Non quadratic equation"
    | d < 0 = []
    | d == 0 = [-b / (2 * a)]
    | otherwise = [(-b - sqrt d) / (2 * a), (-b + sqrt d) / (2 * a)]
    where d = discriminant a b c 

checker a b c x = a * x * x + b * x + c

checker' x f 
    | f x == 0 = True
    | otherwise = False

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs
    