module Lib
    (
        linear,
        quadratic,
    ) where

-- | Решает линейное уравнение. 
-- | Возвращает список, состоящий максимум из одного элемента или вызывается исключение если корней нет.
linear :: (Num i, Eq i, Fractional i) => i -> i -> [i]
linear a b
    | a == 0 && b == 0 = error "Any root (a == b == 0 in ax + b = 0)"
    | a == 0 && b /= 0 = []
    | b == 0           = [0]
    | otherwise        = [-b / a]

-- | Решает квадратное уравнение. 
-- | Возвращает список, состоящий максимум из двух элементов или вызывает исключние если корней нет.
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
