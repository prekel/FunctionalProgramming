module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Enter a, b, c for equation ax^2+bx+c=0 (a = 0 if linear equation): "
    putStrLn "a: "
    line <- getLine
    let a = read line :: Double
    putStrLn "b: "
    line <- getLine
    let b = read line :: Double
    putStrLn "c: "
    line <- getLine
    let c = read line :: Double
    print (quadratic a b c)