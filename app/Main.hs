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