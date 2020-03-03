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
         inputFileName : outputFileName : [] -> do contents <- readFile inputFileName `catch` readHandler
                                                   let allLines = lines contents
                                                   let faculties = stringsToArchipelagos allLines
                                                   menu faculties outputFileName
         _ -> putStrLn "Введите 2 параметра, название входного и выходного файла (могут быть одинаковые и должны существовать)"

printStringWithFlush :: String -> IO ()
printStringWithFlush text = do
    putStr text
    hFlush stdout

readHandler :: IOError -> IO a
readHandler e = printStringWithFlush "Файл не существует" >> exitFailure

checkInput :: String -> Int -> Int -> Bool
checkInput name countIslands countInhabitedIslands
    | name == ""                           = False
    | countIslands < 0 || countInhabitedIslands < 0 = False
    | countInhabitedIslands > countIslands          = False
    | otherwise                            = True


menu :: [Archipelago] -> String -> IO ()
menu archipelagos outputFileName = do
    printStringWithFlush . unlines $ map concatNums choices
    choice <- getLine

    case validate choice of
        Just 1 -> do addArchipelagoToCollectionViaInput archipelagos outputFileName

        Just 2 -> do printStringWithFlush "Введите название архипелага: "
                     name <- getLine
                     modifyArchipelagoInCollectionViaInputByName name archipelagos outputFileName

        Just 3 -> do printStringWithFlush "Введите название архипелага: "
                     name <- getLine
                     do
                        let archipelagos' = deleteArchipelagoByNameFromCollection name archipelagos
                        menu archipelagos' outputFileName

        Just 4 -> do printStringWithFlush (if hasUninhabitedArchipelagoCollection archipelagos then "Имеется\n" else "Не имеется\n")
                     menu archipelagos outputFileName

        Just 5 -> do printStringWithFlush "Введите кол-во островов: "
                     number <- getLine
                     let intNumber = read number :: Int
                     if (intNumber < 0) || (intNumber > length archipelagos)
                            then printStringWithFlush "Некорректный ввод\n"
                            else printStringWithFlush (archipelagosToString (whereCountIslandsIsArchipelagoCollection intNumber archipelagos))
                     menu archipelagos outputFileName

        Just 6 -> printStringWithFlush (archipelagosToString archipelagos) >> menu archipelagos outputFileName

        Just 7 -> do putStrLn "Выход"
                     writeFile outputFileName (archipelagosToString archipelagos)

        Nothing -> do printStringWithFlush "Некорректный ввод\n"
                      menu archipelagos outputFileName
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
    printStringWithFlush "Введите название архипелага: "
    name <- getLine
    printStringWithFlush "Введите кол-во островов: "
    countIslandsStr <- getLine
    let countIslands = read countIslandsStr :: Int
    printStringWithFlush "Введите кол-во обитаемых островов: "
    countInhabitedIslandsStr <- getLine
    let countInhabitedIslands = read countInhabitedIslandsStr :: Int
    if checkInput name countIslands countInhabitedIslands
    then do let archipelago = createArchipelago name countIslands countInhabitedIslands
            let archipelagos' = addArchipelagoToCollection archipelago archipelagos
            menu archipelagos' outputFileName
    else do
        printStringWithFlush "Некорректный ввод\n"
        menu archipelagos outputFileName

modifyArchipelagoInCollectionViaInputByName :: String -> [Archipelago] -> String -> IO ()
modifyArchipelagoInCollectionViaInputByName name archipelagos outputFileName = do
    printStringWithFlush "Введите новое название архипелага: "
    name <- getLine
    printStringWithFlush "Введите новое кол-во островов: "
    countIslandsStr <- getLine
    let countIslands = read countIslandsStr :: Int
    printStringWithFlush "Введите новое кол-во обитаемых островов: "
    countInhabitedIslandsStr <- getLine
    let countInhabitedIslands = read countInhabitedIslandsStr :: Int
    if checkInput name countIslands countInhabitedIslands
    then do let archipelago = createArchipelago name countIslands countInhabitedIslands
            let archipelagos' = modifyArchipelagoByNameFromCollection name archipelago archipelagos
            menu archipelagos' outputFileName
    else do
        printStringWithFlush "Некорректный ввод\n"
        menu archipelagos outputFileName





archipelagosToString :: [Archipelago] -> String
archipelagosToString [] = []
archipelagosToString (f:fs) = archipelago ++ ('\n' : archipelagosToString fs)
    where archipelago = getNameArchipelago f ++ " " ++ show (getCountIslandsArchipelago f) ++ " " ++ show (getCountInhabitedIslandsArchipelago f)


stringsToArchipelagos :: [String] -> [Archipelago]
stringsToArchipelagos [] = []
stringsToArchipelagos (s:ss) = [] ++ [createArchipelago archipelagoName countIslands countInhabitedIslands] ++ stringsToArchipelagos ss
    where [fn, sa, ssa] = words s
          archipelagoName        = fn
          countIslands     = read sa :: Int
          countInhabitedIslands = read ssa :: Int
