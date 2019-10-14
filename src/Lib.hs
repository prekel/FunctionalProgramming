module Lib
    (
    multifactorialRec,
    multifactorialList,
    ) where

-- | Вариант 4

multifactorialRec n k
    | 0 < n && n <= k = n
    | n > k           = n * multifactorialRec (n - k) k
    | otherwise       = error "Нельзя неположительные"

multifactorialList n k
    | n <= 0 || k <= 0 = error "Нельзя неположительные"
    | otherwise        = product [n, n - k .. 1]
