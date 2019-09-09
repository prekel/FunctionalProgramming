module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
    line <- getLine
    putStrLn ("you said: " ++ line)
    let x = read line :: Integer
    print (factorial x)

factorial 0 = 1
factorial n = n * factorial (n - 1)
