import Lib

import Test.Hspec
import Test.QuickCheck

-- | Функции факториала для проверки кратного факториала по свойству разложения факториала через кратный факториал
-- | Разложение факториала через кратный факториал: https://wikimedia.org/api/rest_v1/media/math/render/svg/3c3ece73871dfdfecd7eb8f01c190f66d8812f64

factorialRec n
    | n == 0    = 1
    | n > 0     = n * factorialRec(n - 1)
    | otherwise = error "Нельзя отрицательные"
    
factorialList n
    | n < 0     = error "Нельзя отрицательные"
    | otherwise = product [1..n]

main :: IO ()
main = hspec $ do
    it "Check 5! = 5!!! * 4!!! * 3!!! (recursive)" $ do
        factorialRec 5 `shouldBe` multifactorialRec 5 3 * multifactorialRec 4 3 * multifactorialRec 3 3
    it "Check 5! = 5!!! * 4!!! * 3!!! (list comprehension)" $ do
        factorialList 5 `shouldBe` multifactorialList 5 3 * multifactorialList 4 3 * multifactorialList 3 3
    it "Check 5! = 1 * 2 * 3 * 4 * 5 (recursive)" $ do
        factorialRec 5 `shouldBe` 1 * 2 * 3 * 4 * 5
    it "Check 5! = 1 * 2 * 3 * 4 * 5 (list comprehension)" $ do
        factorialList 5 `shouldBe` 1 * 2 * 3 * 4 * 5
    it "Check 5! = 120 (recursive)" $ do
        factorialRec 5 `shouldBe` 120
    it "Check 5! = 120 (list comprehension)" $ do
        factorialList 5 `shouldBe` 120
    it "Check 10! = 10!! * 9!! (recursive)" $ do
        factorialRec 10 `shouldBe` multifactorialRec 10 2 * multifactorialRec 9 2
    it "Check 10! = 10!! * 9!! (list comprehension)" $ do
        factorialList 10 `shouldBe` multifactorialList 10 2 * multifactorialList 9 2
    it "Check 10! = 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 (recursive)" $ do
        factorialRec 10 `shouldBe` 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10
    it "Check 10! = 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 (list comprehension)" $ do
        factorialRec 10 `shouldBe` 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10
    it "Check 10! = 3628800 (recursive)" $ do
        factorialRec 10 `shouldBe` 3628800
    it "Check 10! = 3628800 (list comprehension)" $ do
        factorialRec 10 `shouldBe` 3628800
