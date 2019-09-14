module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Введите точность eps: "
    line <- getLine
    let eps = read line :: Double

    putStrLn ("Введите через запятую коэффициенты уравнения от коэффициента при большей степени до меньшей: ")
    line <- getLine
    let poly = read ("[" ++ line ++ "]") :: [Double]

    print (polynomialSolve poly eps)

linear :: (Num i, Eq i, Fractional i) => i -> i -> [i]
linear a b
    | a == 0 && b == 0 = error "Infinite number of roots"
    | a == 0 && b /= 0 = []
    | b == 0           = [0]
    | otherwise        = [-b / a]

discriminant :: (Fractional i) => i -> i -> i -> i
discriminant a b c = b ^ 2 - 4 * a * c

quadratic :: (Ord i, Fractional i, Floating i) => i -> i -> i -> [i]
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

polynomialSolve :: (Eq i, Fractional i, Ord i, Floating i, Show i) => [i] -> i -> [i]
polynomialSolve [] _        = error "Empty polynomial"
polynomialSolve [0] _       = error "Any root"
polynomialSolve [x] _       = error "No root"
polynomialSolve [a, b] _    = linear a b
polynomialSolve [a, b, c] _ = quadratic a b c
polynomialSolve p eps       = if not cnd1 then [binarySearch f negativeInf firstRight eps] else [] ++
                              [binarySearch f (fst x) (snd x) eps |
                                  x <- mIntervals',
                                  sign (f (fst x)) * sign (f (snd x)) > 0 && abs (f (fst x) * f (snd x)) >= eps
                              ] ++
                              if not cnd2 then [binarySearch f lastLeft positiveInf eps] else []
    where derRoots          = polynomialSolve (polynomialDerivative p) eps
          mIntervals        = monotonyIntervals (derRoots)
          f x = polynomialCalc p x
          sign x = if x > 0 then 1 else -1
          mIntervals' = take (length mIntervals - 2) (tail mIntervals)
          firstRight = snd (head mIntervals)
          lastLeft = fst (last mIntervals)
          a1 = f (firstRight - 1)
          b1 = firstRight
          cnd1 = (a1 > b1 && b1 > eps) || (a1 < b1 && b1 < -eps)
          a2 = f lastLeft
          b2 = f (lastLeft + 1)
          cnd2 = (a2 > b2 && a2 < -eps) || (a2 < b2 && a2 > eps)

binarySearch :: (Ord t, Ord a, Fractional t, Fractional a, Show a, Show t) => (t -> a) -> t -> t -> t -> t
binarySearch f left right eps
    | left == negativeInf && right == positiveInf = binarySearch f approxNegativeInf approxPositiveInf eps
    | left == negativeInf && sign == 1       = binarySearch f (increaser f (-1) right 1) right eps
    | left == negativeInf && sign == -1      = binarySearch f (increaser f (-1) right (-1)) right eps
    | right == positiveInf && sign == 1      = binarySearch f left (increaser f 1 left (-1)) eps
    | right == positiveInf && sign == -1     = binarySearch f left (increaser f 1 left 1) eps
    | abs (left - right) < eps               = median
    | mValue > 0                             = binarySearch f left median eps
    | otherwise                              = binarySearch f median right eps
    where median = (left + right) / 2
          mValue = sign * f median
          fleft  = if left == negativeInf then f (right - 1) else f left
          fright = if right == positiveInf then f (left + 1) else f right
          sign   = if (fright - fleft) > 0 then 1 else (-1)

increaser :: (Fractional p, Ord p, Fractional t, Ord t, Show t, Show p) => (p -> t) -> p -> p -> t -> p
increaser f k r sign
    | cnd (f rk) == True    = rk
    | (f rk - f r) * sign > 0 = error ("Wrong sign: " ++ (show sign) ++ " k: " ++ show k ++ " r: " ++ show r)
    | otherwise               = increaser f (k * 2) r sign
    where rk    = r + k
          cnd u = if sign == 1 then u < 0 else u > 0

monotonyIntervals :: (Fractional a) => [a] -> [(a, a)]
monotonyIntervals []     = [(negativeInf, positiveInf)]
monotonyIntervals [x]    = [(negativeInf, x), (x, positiveInf)]
monotonyIntervals (x:xs) = [(negativeInf, x)] ++ [(x, snd (head mixs))] ++ tail mixs
    where mixs = monotonyIntervals xs
