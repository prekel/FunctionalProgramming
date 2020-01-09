module Main where

import Data.Matrix

import Matrix

import System.Environment
import Control.Parallel (par)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "ASD"
        (firstMatrixString : secondMatrixString : _) -> do
                                                       let firstMatrix = fromLists (read firstMatrixString :: [[Int]])
                                                           secondMatrix = fromLists (read secondMatrixString :: [[Int]])
                                                           sum = matrixSum firstMatrix secondMatrix
                                                           det = detLaplace sum
                                                           sumString = show (toLists sum)
                                                       putStrLn sumString
                                                       print det

pfib :: Int -> Int
pfib n 
    | n <= 1    = 1
    | otherwise = n1 `par` n2 `seq` n1+n2
    where 
        n1 = pfib (n-1) 
        n2 = pfib (n-2)
        