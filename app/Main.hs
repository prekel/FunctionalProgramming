module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Enter eps (Example: 1e-8): "
    line <- getLine
    let eps = read line :: Double
    putStrLn "Enter coefficients of equation from coefficient at greater degree (Example: 1,-2,-1,2 for x^4-2x^3-x+2=0): "
    line <- getLine
    let poly = read ("[" ++ line ++ "]") :: [Double]
    print ("Roots list: " ++ show (polynomialSolve poly eps))