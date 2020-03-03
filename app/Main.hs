module Main where

import Data.Matrix

import Matrix

import System.Environment
import Control.Parallel (par)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Два аргумента - квадратные матрицы одинакового порядка \n(каждая матрица в квадратных скобках, каждая строка в квадратных скобках, между строками и элементами запятые). \nНапример [[1,2],[3,4]] [[0,-12],[23,12]]"
        (firstMatrixString : secondMatrixString : _) -> do
                                                       let firstMatrix = fromLists (read firstMatrixString :: [[Int]])
                                                           secondMatrix = fromLists (read secondMatrixString :: [[Int]])
                                                           sum = matrixSum firstMatrix secondMatrix
                                                           det = detLaplace sum
                                                           sumString = show (toLists sum)
                                                       putStrLn sumString
                                                       print det
