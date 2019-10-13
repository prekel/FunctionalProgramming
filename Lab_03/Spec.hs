import Lib

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    it "Check 5! = 5!!! * (5 - 1)!!! * (5 - 2)!!! (recursive)" $ do
        factorialRec 5 `shouldBe` multifactorialRec 5 3 * multifactorialRec 4 3 * multifactorialRec 3 3
    it "Check 5! = 5!!! * (5 - 1)!!! * (5 - 2)!!! (list comprehension)" $ do
        factorialList 5 `shouldBe` multifactorialList 5 3 * multifactorialList 4 3 * multifactorialList 3 3
    it "Check 5! = 1 * 2 * 3 * 4 * 5 (recursive)" $ do
        factorialRec 5 `shouldBe` 1 * 2 * 3 * 4 * 5
    it "Check 5! = 1 * 2 * 3 * 4 * 5 (list comprehension)" $ do
        factorialList 5 `shouldBe` 1 * 2 * 3 * 4 * 5
    it "Check 5! = 120 (recursive)" $ do
        factorialRec 5 `shouldBe` 120
    it "Check 5! = 120 (list comprehension)" $ do
        factorialList 5 `shouldBe` 120
    it "Check 10! = 10!! * (10 - 1)!! (recursive)" $ do
        factorialRec 10 `shouldBe` multifactorialRec 10 2 * multifactorialRec 9 2
    it "Check 10! = 10!! * (10 - 1)!! (list comprehension)" $ do
        factorialList 10 `shouldBe` multifactorialList 10 2 * multifactorialList 9 2
    it "Check 10! = 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 (recursive)" $ do
        factorialRec 10 `shouldBe` 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10
    it "Check 10! = 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 (list comprehension)" $ do
        factorialRec 10 `shouldBe` 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10
    it "Check 10! = 3628800 (recursive)" $ do
        factorialRec 10 `shouldBe` 3628800
    it "Check 10! = 3628800 (list comprehension)" $ do
        factorialRec 10 `shouldBe` 3628800
