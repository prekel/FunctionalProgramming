import Test.Hspec
import Test.QuickCheck

import Lib

a1 = createArchipelago "Archipelago1" 2 1

a2 = createArchipelago "Archipelago2" 5 2

a3 = createArchipelago "Archipelago3" 4 1

a4 = createArchipelago "Archipelago4" 5 2

a5 = createArchipelago "Archipelago5" 5 0

c1 = [a1, a2, a3]
c2 = addArchipelagoCollection a4 c1
c3 = modifyNameArchipelagoCollection

main :: IO ()
main = hspec $ do
    it "addArchipelagoCollection" $ do
        [a1, a2, a3, a4] `shouldBe` addArchipelagoCollection a4 c1
        
    