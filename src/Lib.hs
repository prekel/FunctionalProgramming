module Lib
    (
    Archipelago,
    createArchipelago,
    ArchipelagoCollection,
    addArchipelagoCollection,
    deleteArchipelagoCollection,
    modifyNameArchipelagoCollection,
    hasUninhabited,
    whereCountIslandsIs
    ) where

import Data.List
import Data.Maybe

data Archipelago = Archipelago {
    name :: String,
    countIslands :: Int,
    countInhabitedIslands :: Int
} deriving (Show, Eq)

type ArchipelagoCollection = [Archipelago]

createArchipelago name countIslands countInhabitedIslands
    | name == "" = error "Emplty name"
    | countIslands < 2 = error "Islands count must be more than 1"
    | countInhabitedIslands < 0 = error "Inhabited Islands count must be monot less than 0"
    | otherwise = Archipelago {
                     name = name,
                     countIslands = countIslands,
                     countInhabitedIslands = countInhabitedIslands }

anyArchipelagoCollection :: (Archipelago -> Bool) -> ArchipelagoCollection -> Bool
anyArchipelagoCollection = any

whereArchipelagoCollection :: (Archipelago -> Bool) -> ArchipelagoCollection -> ArchipelagoCollection
whereArchipelagoCollection = filter

addArchipelagoCollection :: Archipelago -> ArchipelagoCollection -> ArchipelagoCollection
addArchipelagoCollection archipelago collection = collection ++ [archipelago]

deleteArchipelagoCollection :: Archipelago -> ArchipelagoCollection -> ArchipelagoCollection
deleteArchipelagoCollection archipelago collection = leftListPart ++ tail rightListPart
    where (leftListPart, rightListPart) = splitAt (fromMaybe 0 (elemIndex archipelago collection)) collection

modifyNameArchipelagoCollection :: String -> Archipelago -> ArchipelagoCollection -> ArchipelagoCollection
modifyNameArchipelagoCollection oldName newArchipelago collection = leftListPart ++ [newArchipelago] ++ tail rightListPart
    where archipelago = head (whereArchipelagoCollection (\x -> name x == oldName) collection)
          (leftListPart, rightListPart) = splitAt (fromMaybe 0 (elemIndex archipelago collection)) collection


hasUninhabited collection = anyArchipelagoCollection (\x -> countInhabitedIslands x > 0)

whereCountIslandsIs n = whereArchipelagoCollection (\x -> countIslands x == n)