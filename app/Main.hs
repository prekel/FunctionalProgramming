module Main where

import Lab_05

main :: IO ()
main = do
    putStrLn "Enter 123"
    line <- getLine
    let q = read line :: Double
    print q
