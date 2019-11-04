import Test.Hspec
import Test.QuickCheck

import Lib

a1 = createArchipelago "Archipelago1" 2 1

a2 = createArchipelago "Archipelago2" 5 2

a3 = createArchipelago "Archipelago3" 4 1

a4 = createArchipelago "Archipelago4" 5 2

a5 = createArchipelago "Archipelago5" 5 0

c1 = createArchipelagoCollection [a1, a2, a3]
c2 = createArchipelagoCollection [a1, a2, a3, a4]
c3 = createArchipelagoCollection [a5, a2, a3]
c4 = createArchipelagoCollection [a2, a3]
c5 = createArchipelagoCollection [a1]
c6 = createArchipelagoCollection [a1, a2, a3, a4, a5]
c7 = createArchipelagoCollection [a2, a4, a5]
c8 = createArchipelagoCollection []

main :: IO ()
main = hspec $ do
    it "addArchipelagoCollection test 1" $ do
        addArchipelagoCollection a4 c1 `shouldBe` c2
    it "deleteArchipelagoCollection test 1" $ do
        deleteArchipelagoCollection a1 c1 `shouldBe` c4
    it "deleteArchipelagoCollection test 2" $ do
       deleteArchipelagoCollection a4 c1 `shouldBe` c1
    it "modifyNameArchipelagoCollection test 1" $ do
       modifyNameArchipelagoCollection "Archipelago1" a5 c1 `shouldBe` c3
    it "hasUninhabitedArchipelagoCollection test 1" $ do
        hasUninhabitedArchipelagoCollection c1 `shouldBe` True
    it "whereCountIslandsIsArchipelagoCollection test 1" $ do
        whereCountIslandsIsArchipelagoCollection 1 c1 `shouldBe` c8
    it "whereCountIslandsIsArchipelagoCollection test 2" $ do
        whereCountIslandsIsArchipelagoCollection 5 c6 `shouldBe` c7
    it "getNameArchipelago test 1" $ do
        getNameArchipelago a1 `shouldBe` "Archipelago1"
    it "getCountIslandsArchipelago test 1" $ do
        getCountIslandsArchipelago a1 `shouldBe` 2
    it "getCountInhabitedIslandsArchipelago test 1" $ do
        getCountInhabitedIslandsArchipelago a1 `shouldBe` 1

