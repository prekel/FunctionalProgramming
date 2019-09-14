module Main where

import Lib

main :: IO ()
main = do
    line <- getLine
    let a = read line :: Double
    line <- getLine
    let b = read line :: Double
    line <- getLine
    let c = read line :: Double
    print (quadratic a b c)

linear :: (Num i, Eq i, Fractional i) => i -> i -> [i]
linear a b
    | a == 0 && b == 0 = error "Infinite number of roots"
    | a == 0 && b /= 0 = []
    | b == 0           = [0]
    | otherwise        = [-b / a]

discriminant a b c = b ^ 2 - 4 * a * c

quadratic a b c
    | a == 0    = linear b c
--    | a == 0 = error "Non quadratic equation"
    | d < 0     = []
    | d == 0    = [-b / (2 * a)]
    | otherwise = [(-b - sqrt d) / (2 * a), (-b + sqrt d) / (2 * a)]
    where d = discriminant a b c

positiveInf :: Fractional a => a
positiveInf = 1 / 0
negativeInf :: Fractional a => a
negativeInf = -1 / 0

approxPositiveInf :: Fractional a => a
approxPositiveInf = 2^32
approxNegativeInf :: Fractional a => a
approxNegativeInf = -2^32

polynomialCalc :: (Num i) => [i] -> i -> i
polynomialCalc [] x     = 0
polynomialCalc [g] x    = g
polynomialCalc (g:gs) x = g * x ^ length gs + polynomialCalc gs x

polynomialDerivative :: (Num i) => [i] -> [i]
polynomialDerivative []     = []
polynomialDerivative [x]    = []
polynomialDerivative (x:xs) = (x * fromIntegral(length xs)) : polynomialDerivative xs

polynomialSolve :: (Eq i, Fractional i, Ord i) => [i] -> i -> [i]
polynomialSolve [] _       = error "Empty polynomial"
polynomialSolve [0] _      = error "Any root"
polynomialSolve [x] _      = error "No root"
polynomialSolve (a:b:[]) _ = linear a b
polynomialSolve p eps      = [binarySearch polynomialCalc' (fst x) (snd x) eps | x <- mIntervals]
    where derRoots = polynomialSolve (polynomialDerivative p) eps
          mIntervals = monotonyIntervals (derRoots)
          polynomialCalc' x = polynomialCalc p x

binarySearch :: (Ord t, Ord a, Fractional t, Num a) => (t -> a) -> t -> t -> t -> t
binarySearch f left right eps
    | left == negativeInf      = binarySearch f approxNegativeInf right eps
    | right == positiveInf     = binarySearch f left approxPositiveInf eps
    | abs (left - right) < eps = median
    | mValue > 0               = binarySearch f left median eps
    | otherwise                = binarySearch f median right eps
    where median = (left + right) / 2
          mValue = isPositive * f median
          isPositive = if (f right - f left) > 0 then 1 else (-1)

monotonyIntervals :: (Fractional a) => [a] -> [(a, a)]
monotonyIntervals []     = [(negativeInf, positiveInf)]
monotonyIntervals [x]    = [(negativeInf, x), (x, positiveInf)]
monotonyIntervals (x:xs) = [(negativeInf, x)] ++ [(x, snd (head mixs))] ++ tail mixs
    where mixs = monotonyIntervals xs

--monotonyIntervals' :: (Fractional a) => [a] -> [(a, a)]
--monotonyIntervals' x = take (length mix - 2) (tail (mix))
--    where mix = monotonyIntervals x