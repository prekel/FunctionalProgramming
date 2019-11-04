module Lib
    (
    Archipelago,
    createArchipelago,
    ArchipelagoCollection,
    createArchipelagoCollection,
    getNameArchipelago,
    getCountIslandsArchipelago,
    getCountInhabitedIslandsArchipelago,
    addArchipelagoCollection,
    deleteArchipelagoCollection,
    modifyNameArchipelagoCollection,
    hasUninhabitedArchipelagoCollection,
    whereCountIslandsIsArchipelagoCollection
    ) where

import Data.List
import Data.Maybe

-- | Архипелаг
data Archipelago = Archipelago {
    -- | Название архипелагов
    name :: String,
    -- | Кол-во островов
    countIslands :: Int,
    -- | Кол-во населённых островов
    countInhabitedIslands :: Int
} deriving (Show, Eq)

-- | Коллекция архипелагов
type ArchipelagoCollection = [Archipelago]

-- | Создаёт архипелаг, проверяя поляна соответствия значениям
createArchipelago name countIslands countInhabitedIslands
    | name == "" = error "Emplty name"
    | countIslands < 2 = error "Islands count must be more than 1"
    | countInhabitedIslands < 0 = error "Inhabited Islands count must be monot less than 0"
    | otherwise = Archipelago {
                     name = name,
                     countIslands = countIslands,
                     countInhabitedIslands = countInhabitedIslands }

-- | Получение названия архипелага
getNameArchipelago (Archipelago name _ _) = name
-- | Получение кол-ва островов архипелага
getCountIslandsArchipelago (Archipelago _ countIslands _) = countIslands
-- | Получение кол-ва обитаемых островов архипелага
getCountInhabitedIslandsArchipelago (Archipelago _ _ countInhabitedIslands) = countInhabitedIslands

-- | Создаёт коллекция архипелагов из списка
createArchipelagoCollection :: [Archipelago] -> ArchipelagoCollection
createArchipelagoCollection list = list

-- | Функция высшего порядка выясняющая есть ли хотя бы один элемент в коллекции удолетворяющий условию
anyArchipelagoCollection :: (Archipelago -> Bool) -> ArchipelagoCollection -> Bool
anyArchipelagoCollection = any

-- | Функция высшего порядка составляющая коллекцию архипелаго, удолетворяющих условию
whereArchipelagoCollection :: (Archipelago -> Bool) -> ArchipelagoCollection -> ArchipelagoCollection
whereArchipelagoCollection = filter

-- | Создаёт новую коллекция с добавленным в него архипелагом
addArchipelagoCollection :: Archipelago -> ArchipelagoCollection -> ArchipelagoCollection
addArchipelagoCollection archipelago collection = collection ++ [archipelago]

-- | Создаёт новую коллекцию с удалённым из него архипелагом
deleteArchipelagoCollection :: Archipelago -> ArchipelagoCollection -> ArchipelagoCollection
deleteArchipelagoCollection archipelago collection
    | archipelago `elem` collection = leftListPart ++ tail rightListPart
    | otherwise = collection
    where (leftListPart, rightListPart) = splitAt (fromMaybe 0 (elemIndex archipelago collection)) collection

-- | Создаёт новую коллекцию с заменённым архипелагом по названию
modifyNameArchipelagoCollection :: String -> Archipelago -> ArchipelagoCollection -> ArchipelagoCollection
modifyNameArchipelagoCollection oldName newArchipelago collection = leftListPart ++ [newArchipelago] ++ tail rightListPart
    where archipelago = head (whereArchipelagoCollection (\x -> name x == oldName) collection)
          (leftListPart, rightListPart) = splitAt (fromMaybe 0 (elemIndex archipelago collection)) collection

-- | Выясняет, имеются ли архипелаги, состоящие только из необитаемых островов
hasUninhabitedArchipelagoCollection = anyArchipelagoCollection (\x -> countInhabitedIslands x > 0)

-- | Получает коллекцию архипелагов с указанием кол-ва островов в них
whereCountIslandsIsArchipelagoCollection n = whereArchipelagoCollection (\x -> countIslands x == n)
