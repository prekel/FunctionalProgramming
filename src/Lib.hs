module Lib
    (
--        secondElement,
--        thirdElement,
--        fourthElement,
--        longerThan,
        distr4,
        distr2,
        distr1,
--        distr,
--        swapFirstLast,
    ) where

---- | Вариант 1
--secondElement a = head (tail a)
--thirdElement a = head (tail (tail a))
--fourthElement a = head (tail (tail (tail a)))
--
---- | Вариант 2 (longer_than)
--longerThan a b = sum [1 | x <- a] > sum [1 | x <- b]

-- | Вариант 3
distr4 ar1 ar2 ar3 ar4 = [[ar1, ar2], [ar3, ar4]]
distr2 ar1 ar2 = [[ar1], [ar2]]
distr1 ar1 = [[ar1]]
--distr [ar1, ar2, ar3, ar4] = [[ar1, ar2], [ar3, ar4]]
--distr [ar1, ar2] = [[ar1], [ar2]]
--distr [ar1] = [[ar1]]

distr' [] = []
distr' [ar1, ar2, ar3, ar4] = distr4 ar1 ar2 ar3 ar3
distr' [ar1, ar2] = distr2 ar1 ar2
distr' [ar1] = distr1 ar1
distr' (x:xs) = []

---- | Вариант 4 (swap-first-last)
--swapFirstLast list = swapFirstLast' [] list
--    where
--        swapFirstLast' a [] = []
--        swapFirstLast' a [x] = [x]
--        swapFirstLast' a [x, y] = [y] ++ a ++ [x]
--        swapFirstLast' a b = swapFirstLast' (a ++ [second]) (first : tailTail)
--            where
--                first = head b
--                second = head (tail b)
--                tailTail = tail (tail b)
