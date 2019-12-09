module Lab_06
    (

    ) where

import Archipelago
import ArchipelagoCollection


menu :: [Archipelago] -> IO ()
menu faculties = do
    putStr . unlines $ map concatNums choices
    choice <- getLine

    case validate choice of
        Just 1 -> do addArchipelagoToCollectionViaInput faculties

        Just 2 -> do putStr "Input number of faculty in the list to modify: "
                     name <- getLine
                     modifyArchipelagoInCollectionViaInputByName name faculties

        Just 3 -> do putStr "Input number of faculty in the list to delete: "
                     name <- getLine
                     do
                        let faculties' = deleteArchipelagoByNameFromCollection name faculties
                        menu faculties'

        Just 4 -> do putStrLn (if hasUninhabitedArchipelagoCollection faculties then "+" else "-")
                     menu faculties

        Just 5 -> do putStr "Input N: "
                     number <- getLine
                     let intNumber = read number :: Int
                     if (intNumber < 0) || (intNumber > length faculties)
                            then putStrLn "Incorrect input"
                            else putStr (archipelagoToString (whereCountIslandsIsArchipelagoCollection intNumber faculties))
                     menu faculties

        Just 6 -> putStr (archipelagoToString faculties) >> menu faculties

        Just 7 -> do putStr "Input path to the file: "
                     filePath <- getLine
                     writeFile filePath (archipelagoToString faculties) >> menu faculties

        Just 8 -> do putStr "Input path to the file: "
                     filePath <- getLine
                     contents <- readFile filePath
                     let allLines = lines contents
                     let faculties' = stringsToArchipelagos allLines
                     menu faculties'

        Just 9 -> putStrLn "Exit the menu"

        Nothing -> putStrLn "Incorrect input"
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
    ("Create and add a faculty to the list"),
    ("Modify a faculty in the list"),
    ("Delete a faculty in the list"),
    ("Sort faculties by the fewest amount of students"),
    ("Sort faculties by the best relative academic performance"),
    ("Display all faculties in the list"),
    ("Write faculties into the file"),
    ("Read faculties from the file"),
    ("Exit")
 ]

addArchipelagoToCollectionViaInput :: [Archipelago] -> IO ()
addArchipelagoToCollectionViaInput faculties = do
    putStr "Input new name of the faculty: "
    facultyName <- getLine
    putStr "Enter new amount of students:"
    studentsAmount <- getLine
    let intStudentAmount = read studentsAmount :: Int
    putStr "Enter new amount of successful students: "
    successfulStudentsAmount <- getLine
    let intSuccessfulStudentsAmount = read successfulStudentsAmount :: Int
    if checkInput facultyName intStudentAmount intSuccessfulStudentsAmount
    then do let faculty = createArchipelago facultyName intStudentAmount intSuccessfulStudentsAmount
            let faculties' = addArchipelagoToCollection faculty faculties
            menu faculties'
    else do
        putStrLn "Incorrect input"
        menu faculties

modifyArchipelagoInCollectionViaInputByName :: String -> [Archipelago] -> IO ()
modifyArchipelagoInCollectionViaInputByName name faculties = do
    putStr "Input new name of the faculty: "
    facultyName <- getLine
    putStr "Enter new amount of students:"
    studentsAmount <- getLine
    let intStudentAmount = read studentsAmount :: Int
    putStr "Enter new amount of successful students: "
    successfulStudentsAmount <- getLine
    let intSuccessfulStudentsAmount = read successfulStudentsAmount :: Int
    if checkInput facultyName intStudentAmount intSuccessfulStudentsAmount
    then do let archipelago = createArchipelago facultyName intStudentAmount intSuccessfulStudentsAmount
            let faculties' = modifyArchipelagoByNameFromCollection name archipelago faculties
            menu faculties'
    else do
        putStrLn "Incorrect input"
        menu faculties

checkInput :: String -> Int -> Int -> Bool
checkInput name studAmount succStudAmount
    | name == []                           = False
    | studAmount < 0 || succStudAmount < 0 = False
    | succStudAmount > studAmount          = False
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
