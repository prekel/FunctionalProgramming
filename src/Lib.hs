module Lib
    (
        linear,
        quadratic,
        polynomialCalc,
        polynomialDerivative,
        polynomialSolve,
    ) where

-- | Решает линейное уравнение.
linear :: (Num i, Eq i, Fractional i) => i -> i -> [i]
linear a b
    | a == 0 && b == 0 = error "Any root (a == b == 0 in ax + b = 0)"
    | a == 0 && b /= 0 = []
    | b == 0           = [0]
    | otherwise        = [-b / a]

-- | Решает квадратное уравнение.
quadratic :: (Ord i, Fractional i, Floating i) => i -> i -> i -> [i]
quadratic a b c
    | a == 0    = linear b c
--    | a == 0 = error "Non quadratic equation"
    | d < 0     = []
    | d == 0    = [x0]
    | otherwise = [min x1 x2, max x1 x2]
    where discriminant a b c = b ^ 2 - 4 * a * c
          d = discriminant a b c
          x0 = -b / (2 * a)
          x1 = (-b - sqrt d) / (2 * a)
          x2 = (-b + sqrt d) / (2 * a)

-- | Положительная бесконечность.
positiveInf :: Fractional a => a
positiveInf = 1 / 0
-- | Отрицательная бесконечность.
negativeInf :: Fractional a => a
negativeInf = -1 / 0

-- | Очень большое число.
approxPositiveInf :: Fractional a => a
approxPositiveInf = 2^62
-- | Очень маленькое число.
approxNegativeInf :: Fractional a => a
approxNegativeInf = -2^64

-- | Вычисляет значения многочлена по его коэффициентам.
polynomialCalc :: (Num i) => [i] -> i -> i
polynomialCalc [] x     = 0
polynomialCalc [g] x    = g
polynomialCalc (g:gs) x = g * x ^ length gs + polynomialCalc gs x

-- | Находит коэффициенты производной заданного многочлена.
polynomialDerivative :: (Num i) => [i] -> [i]
polynomialDerivative []     = []
polynomialDerivative [x]    = []
polynomialDerivative (x:xs) = (x * fromIntegral(length xs)) : polynomialDerivative xs

-- | Вычисляет корни алгебраиеского уравнения. Если степерь уравнения больше двух, корни вычисляються численно с 
-- | заданной точностью. Если уравнение квадратное или линейное, точность игнорируется и корни вычисляются по формулам. 
-- | 'p' - список коэффициентов уравнения от коэффициента при большей степень до меньшей
-- | 'eps' - точность вычисления
-- | Возвращает список корней отсортированный в порядке возрастания. Если корней бесконечное количество, 
-- | вызывается исключение с соответствующим сообщением.
polynomialSolve :: (Eq i, Fractional i, Ord i, Floating i, Show i) => [i] -> i -> [i]
polynomialSolve [] _        = error "Any root (no coefficients)"
polynomialSolve [0] _       = error "Any root (0 = 0)"
polynomialSolve [x] _       = []
polynomialSolve [a, b] _    = linear a b
polynomialSolve [a, b, c] _ = quadratic a b c
polynomialSolve p eps = [u | u <- [binarySearch f (fst x) (snd x) eps | x <- mIntervals], abs (f u) <= eps * 100]
    where
        derRoots = polynomialSolve (polynomialDerivative p) eps
        mIntervals = monotonyIntervals derRoots
        f = polynomialCalc p
        sign x = if x > 0 then 1 else -1

-- | На отрезке от 'left' до 'right' находит, где функция f равна нулю.
binarySearch :: (Ord t, Fractional t, Show t) => (t -> t) -> t -> t -> t -> t
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
          fLeft  = if left == negativeInf then f (right - 1) else f left
          fRight = if right == positiveInf then f (left + 1) else f right
          sign   = if (fRight - fLeft) > 0 then 1 else (-1)
          increaser :: (Fractional p, Ord p, Fractional t, Ord t, Show t, Show p) => (p -> t) -> p -> p -> t -> p
          increaser f k r sign
              | cnd (f rk) = rk
              | (f rk - f r) * sign > 0 = error ("Wrong sign: " ++ show sign ++ " k: " ++ show k ++ " r: " ++ show r)
              | otherwise = increaser f (k * 2) r sign
              where rk = r + k
                    cnd u = if sign == 1 then u < 0 else u > 0

-- | Из списка точек экстремума формирует список интервалов монотонности.
monotonyIntervals :: (Fractional a) => [a] -> [(a, a)]
monotonyIntervals []     = [(negativeInf, positiveInf)]
monotonyIntervals [x]    = [(negativeInf, x), (x, positiveInf)]
monotonyIntervals (x:xs) = [(negativeInf, x)] ++ [(x, snd (head mixs))] ++ tail mixs
    where mixs = monotonyIntervals xs

