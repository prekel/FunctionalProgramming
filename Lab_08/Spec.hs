import Test.Hspec
import Test.QuickCheck

import Data.Matrix

import Matrix

main :: IO ()
main = hspec $ do
    it "m1 det1 test" $ do
        matrixDetParallel m1 `shouldBe` 0
    it "m1 det2 test" $ do
        matrixDetParallel m1 `shouldBe` detLaplace m1
    it "m21+m22 sum test" $ do
        matrixSum m21 m22 `shouldBe` m2sum
    it "m2sum det1 test" $ do
        matrixDetParallel m2sum `shouldBe` 276
    it "m2sum det2 test" $ do
        matrixDetParallel m2sum `shouldBe` detLaplace m2sum
    where
        m1 = fromLists [[1,2,3],[4,5,6],[7,8,9]]
        m21 = fromLists [[1,2],[3,4]]
        m22 = fromLists [[0,-12],[23,12]]
        m2sum = fromLists [[1,-10],[26,16]]
