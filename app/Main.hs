module Main where

import Archipelago
import ArchipelagoCollection
import Prelude hiding (catch)
import Data.List (sortBy)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import System.Exit
import Control.Exception
import Control.DeepSeq

main :: IO ()
main = do
     args <- getArgs
     case args of
         inputFilePath : outputFilePath : [] -> do contents <- (readFile outputFilePath) `catch` readHandler
                                                   evaluate (force contents)
                                                   contents <- (readFile inputFilePath) `catch` readHandler
                                                   let allLines = lines contents
                                                   let faculties = stringsToArchipelagos allLines
                                                   menu faculties outputFilePath
         _ -> putStrLn "Enter two arguments, the first one is path to the input file, the second one is path to the output file"

prompt :: String -> IO ()
prompt text = do
    putStr text
    hFlush stdout

readHandler :: IOError -> IO a
readHandler e = putStrLn "The file does not exist" >> exitFailure

checkInput :: String -> Int -> Int -> Bool
checkInput name countIslands countInhabitedIslands
    | name == ""                           = False
    | countIslands < 0 || countInhabitedIslands < 0 = False
    | countInhabitedIslands > countIslands          = False
    | otherwise                            = True


menu :: [Archipelago] -> String -> IO ()
menu archipelagos outputFileName = do
    putStr . unlines $ map concatNums choices
    choice <- getLine

    case validate choice of
        Just 1 -> do addArchipelagoToCollectionViaInput archipelagos outputFileName

        Just 2 -> do putStr "Введите название архипелага: "
                     name <- getLine
                     modifyArchipelagoInCollectionViaInputByName name archipelagos outputFileName

        Just 3 -> do putStr "Введите название архипелага: "
                     name <- getLine
                     do
                        let archipelagos' = deleteArchipelagoByNameFromCollection name archipelagos
                        menu archipelagos' outputFileName

        Just 4 -> do putStrLn (if hasUninhabitedArchipelagoCollection archipelagos then "Имеется" else "Не имеется")
                     menu archipelagos outputFileName

        Just 5 -> do putStr "Введите кол-во островов: "
                     number <- getLine
                     let intNumber = read number :: Int
                     if (intNumber < 0) || (intNumber > length archipelagos)
                            then putStrLn "Некорректный ввод"
                            else putStr (archipelagoToString (whereCountIslandsIsArchipelagoCollection intNumber archipelagos))
                     menu archipelagos outputFileName

        Just 6 -> putStr (archipelagoToString archipelagos) >> menu archipelagos outputFileName

        Just 7 -> putStrLn "Выход"

        Nothing -> putStrLn "Некорректный ввод"
    where concatNums (i, s) = show i ++ ") " ++ s

validate :: String -> Maybe Int
validate s = isValid (reads s)
    where isValid []        = Nothing
          isValid ((n, _):_)
            | outOfBounds n = Nothing
            | otherwise     = Just n
          outOfBounds n = (n < 1) || (n > length choices)

choices :: [(Int, String)]
choices = zip [1.. ] [
    "Создать и добавить архипелаг",
    "Модифицировать архипелаг",
    "Удалить архипелаг",
    "Имеются ли архипелаги, состоящие только из необитаемых островов",
    "Вывести архипелаги с указанным кол-вом островов в них",
    "Вывести все архипелаги",
    "Выйти"
 ]

addArchipelagoToCollectionViaInput :: [Archipelago] -> String -> IO ()
addArchipelagoToCollectionViaInput archipelagos outputFileName = do
    putStr "Введите название архипелага: "
    name <- getLine
    putStr "Введите кол-во островов: "
    countIslandsStr <- getLine
    let countIslands = read countIslandsStr :: Int
    putStr "Введите кол-во обитаемых островов: "
    countInhabitedIslandsStr <- getLine
    let countInhabitedIslands = read countInhabitedIslandsStr :: Int
    if checkInput name countIslands countInhabitedIslands
    then do let archipelago = createArchipelago name countIslands countInhabitedIslands
            let archipelagos' = addArchipelagoToCollection archipelago archipelagos
            menu archipelagos' outputFileName
    else do
        putStrLn "Некорректный ввод"
        menu archipelagos outputFileName

modifyArchipelagoInCollectionViaInputByName :: String -> [Archipelago] -> String -> IO ()
modifyArchipelagoInCollectionViaInputByName name archipelagos outputFileName = do
    putStr "Введите новое название архипелага: "
    name <- getLine
    putStr "Введите новое кол-во островов: "
    countIslandsStr <- getLine
    let countIslands = read countIslandsStr :: Int
    putStr "Введите новое кол-во обитаемых островов: "
    countInhabitedIslandsStr <- getLine
    let countInhabitedIslands = read countInhabitedIslandsStr :: Int
    if checkInput name countIslands countInhabitedIslands
    then do let archipelago = createArchipelago name countIslands countInhabitedIslands
            let archipelagos' = modifyArchipelagoByNameFromCollection name archipelago archipelagos
            menu archipelagos' outputFileName
    else do
        putStrLn "Некорректный ввод"
        menu archipelagos outputFileName





archipelagoToString :: [Archipelago] -> String
archipelagoToString [] = []
archipelagoToString (f:fs) = archipelago ++ ('\n' : archipelagoToString fs)
    where archipelago = getNameArchipelago f ++ " " ++ show (getCountIslandsArchipelago f) ++ " " ++ show (getCountInhabitedIslandsArchipelago f)


stringsToArchipelagos :: [String] -> [Archipelago]
stringsToArchipelagos [] = []
stringsToArchipelagos (s:ss) = [] ++ [createArchipelago archipelagoName countIslands countInhabitedIslands] ++ stringsToArchipelagos ss
    where [fn, sa, ssa] = words s
          archipelagoName        = fn
          countIslands     = read sa :: Int
          countInhabitedIslands = read ssa :: Int
