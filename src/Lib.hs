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
import Prelude hiding (Functor, fmap, Applicative,  pure, (<*>))

-- | Архипелаг
data Archipelago a = Archipelago {
    -- | Название архипелагов
    name :: String,
    -- | Кол-во островов
    countIslands :: Int,
    -- | Кол-во населённых островов
    countInhabitedIslands :: Int
} | Function a deriving (Show, Eq)

-- | Коллекция архипелагов
--type ArchipelagoCollection = [Archipelago]

-- Объявление функтора
class Functor f where
    fmap :: (Int -> Int) -> f a -> f b

-- Создание экземпляра функтора
instance Functor Archipelago where
    fmap f (Archipelago name countIslands countInhabitedIslands) = Archipelago name (f countIslands) (f countInhabitedIslands)

-- Объявление аппликативного функтора
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (Int -> Int) -> f a -> f b

-- Создание экземпляра аппликативного функтора
instance Applicative Archipelago where
    pure a = Function a
    Function f <*> Archipelago n a b = Archipelago n (f a) (f b)

type ArchipelagoCollection a = [Archipelago a]

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
createArchipelagoCollection :: [Archipelago a] -> ArchipelagoCollection a
createArchipelagoCollection list = list

-- | Функция высшего порядка выясняющая есть ли хотя бы один элемент в коллекции удолетворяющий условию
anyArchipelagoCollection :: (Archipelago a -> Bool) -> ArchipelagoCollection a -> Bool
anyArchipelagoCollection = any

-- | Функция высшего порядка составляющая коллекцию архипелаго, удолетворяющих условию
whereArchipelagoCollection :: (Archipelago a -> Bool) -> ArchipelagoCollection a -> ArchipelagoCollection a
whereArchipelagoCollection = filter

-- | Создаёт новую коллекция с добавленным в него архипелагом
addArchipelagoCollection :: Archipelago a -> ArchipelagoCollection a -> ArchipelagoCollection a
addArchipelagoCollection archipelago collection = collection ++ [archipelago]

-- | Создаёт новую коллекцию с удалённым из него архипелагом
deleteArchipelagoCollection :: Archipelago a -> ArchipelagoCollection a -> ArchipelagoCollection a
deleteArchipelagoCollection archipelago collection
    | archipelago `elem` collection = leftListPart ++ tail rightListPart
    | otherwise = collection
    where (leftListPart, rightListPart) = splitAt (fromMaybe 0 (elemIndex archipelago collection)) collection

-- | Создаёт новую коллекцию с заменённым архипелагом по названию
modifyNameArchipelagoCollection :: String -> Archipelago a -> ArchipelagoCollection a -> ArchipelagoCollection a
modifyNameArchipelagoCollection oldName newArchipelago collection = leftListPart ++ [newArchipelago] ++ tail rightListPart
    where archipelago = head (whereArchipelagoCollection (\x -> name x == oldName) collection)
          (leftListPart, rightListPart) = splitAt (fromMaybe 0 (elemIndex archipelago collection)) collection

-- | Выясняет, имеются ли архипелаги, состоящие только из необитаемых островов
hasUninhabitedArchipelagoCollection = anyArchipelagoCollection (\x -> countInhabitedIslands x > 0)

-- | Получает коллекцию архипелагов с указанием кол-ва островов в них
whereCountIslandsIsArchipelagoCollection n = whereArchipelagoCollection (\x -> countIslands x == n)
