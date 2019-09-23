import Lib

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

eps = 1e-8
f1 x = 1 * x * x * x - 2 * x * x - 1 * x + 2
p1 = [1, -2, -1, 2]
a1 = polynomialSolve p1 eps

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exc   eption if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
      
    it ("1231e12" ++ show p1 ++ " " ++ show a1) $ do
       f1 (head a1) < eps `shouldBe` True
