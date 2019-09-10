module Lib
    (
--    ( someFunc
    ) where

--someFunc :: IO ()
--someFunc = do
--    line <- getLine
 --   putStrLn ("you said: " ++ line)
 --   let x = read line :: Integer
--    print (factorial x)
--    print (linear x 6)

factorial 0 = 1
factorial n = n * factorial (n - 1)

--linear :: (Num a, Num a) => a -> a -> [a]
--linear a b
--    | a * b == 0 = [-b / a]
--    | otherwise = []

