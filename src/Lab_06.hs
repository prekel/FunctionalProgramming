module Lab_06
    (
        menu
    ) where

import Archipelago
import ArchipelagoCollection


menu :: [Archipelago] -> IO ()
menu archipelago = do
    putStr . unlines $ map concatNums choices
    choice <- getLine

    case validate choice of
        Just 1 -> do addArchipelagoToCollectionViaInput archipelago

        Just 2 -> do putStr "Введите название архипелага: "
                     name <- getLine
                     modifyArchipelagoInCollectionViaInputByName name archipelago

        Just 3 -> do putStr "Введите название архипелага: "
                     name <- getLine
                     do
                        let archipelago' = deleteArchipelagoByNameFromCollection name archipelago
                        menu archipelago'

        Just 4 -> do putStrLn (if hasUninhabitedArchipelagoCollection archipelago then "Имеется" else "Не имеется")
                     menu archipelago

        Just 5 -> do putStr "Введите кол-во островов: "
                     number <- getLine
                     let intNumber = read number :: Int
                     if (intNumber < 0) || (intNumber > length archipelago)
                            then putStrLn "Некорректный ввод"
                            else putStr (archipelagoToString (whereCountIslandsIsArchipelagoCollection intNumber archipelago))
                     menu archipelago

        Just 6 -> putStr (archipelagoToString archipelago) >> menu archipelago

        Just 7 -> do putStr "Введите название файла: "
                     filePath <- getLine
                     writeFile filePath (archipelagoToString archipelago) >> menu archipelago

        Just 8 -> do putStr "Введите название файла: "
                     filePath <- getLine
                     contents <- readFile filePath
                     let allLines = lines contents
                     let archipelagos' = stringsToArchipelagos allLines
                     menu archipelagos'

        Just 9 -> putStrLn "Выход"

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
    "Вывести архипелаги с указанием кол-ва островов в них",
    "Вывесли все архипелаги",
    "Записать в файл",
    "Считать из файла",
    "Выйти"
 ]

addArchipelagoToCollectionViaInput :: [Archipelago] -> IO ()
addArchipelagoToCollectionViaInput archipelagos = do
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
            menu archipelagos'
    else do
        putStrLn "Некорректный ввод"
        menu archipelagos

modifyArchipelagoInCollectionViaInputByName :: String -> [Archipelago] -> IO ()
modifyArchipelagoInCollectionViaInputByName name archipelagos = do
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
            menu archipelagos'
    else do
        putStrLn "Некорректный ввод"
        menu archipelagos

checkInput :: String -> Int -> Int -> Bool
checkInput name countIslands countInhabitedIslands
    | name == ""                           = False
    | countIslands < 0 || countInhabitedIslands < 0 = False
    | countInhabitedIslands > countIslands          = False
    | otherwise                            = True

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
