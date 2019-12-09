module ArchipelagoCollection
    (
    addArchipelagoToCollection,
    deleteArchipelagoFromCollection,
    modifyArchipelagoByNameFromCollection,
    hasUninhabitedArchipelagoCollection,
    whereCountIslandsIsArchipelagoCollection
    ) where

import Archipelago
import Data.Maybe (fromMaybe)
import Data.List (elemIndex, any, filter)

addArchipelagoToCollection :: Archipelago -> [Archipelago] -> [Archipelago]
addArchipelagoToCollection f archipelagos = archipelagos ++ [f]

-- | Создаёт новую коллекцию с удалённым из него архипелагом
deleteArchipelagoFromCollection :: Archipelago -> [Archipelago] -> [Archipelago]
deleteArchipelagoFromCollection archipelago collection
    | archipelago `elem` collection = leftListPart ++ tail rightListPart
    | otherwise = collection
    where (leftListPart, rightListPart) = splitAt (fromMaybe 0 (elemIndex archipelago collection)) collection

-- | Создаёт новую коллекцию с заменённым архипелагом по названию
modifyArchipelagoByNameFromCollection oldName newArchipelago collection = leftListPart ++ [newArchipelago] ++ tail rightListPart
    where archipelago = head (filter (\x -> getNameArchipelago x == oldName) collection)
          (leftListPart, rightListPart) = splitAt (fromMaybe 0 (elemIndex archipelago collection)) collection

-- | Выясняет, имеются ли архипелаги, состоящие только из необитаемых островов
hasUninhabitedArchipelagoCollection a = any (\x -> getCountInhabitedIslandsArchipelago x > 0)

-- | Получает коллекцию архипелагов с указанием кол-ва островов в них
whereCountIslandsIsArchipelagoCollection n = filter (\x -> getCountIslandsArchipelago x == n)
