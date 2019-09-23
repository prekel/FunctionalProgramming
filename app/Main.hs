module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Enter 123"
    line <- getLine
    let q = read line :: Double
    print q
