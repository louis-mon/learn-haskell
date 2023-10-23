module LibSpec where

import Test.Hspec
import Test.QuickCheck
import Lib

spec :: Spec
spec = do
  describe "Test 1" $ do
    it "zip works" $ do
      zipWith2d (+) [[1, 2], [5, 10]] [[100, 200], [0, 10]] `shouldBe` [[101, 202], [5, 20]]
    
    it "zip keeps size" $ do
      property $ \x -> (read . show) x `shouldBe` (x :: Int)


