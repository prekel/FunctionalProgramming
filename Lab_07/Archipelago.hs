module Archipelago
    (
    Archipelago,
    createArchipelago,
    getNameArchipelago,
    getCountIslandsArchipelago,
    getCountInhabitedIslandsArchipelago,
    ) where

-- | Архипелаг
data Archipelago = Archipelago {
    -- | Название архипелагов
    name :: String,
    -- | Кол-во островов
    countIslands :: Int,
    -- | Кол-во населённых островов
    countInhabitedIslands :: Int
} deriving (Show, Eq)

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
