module Ch02Spec
  ( main
  , spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Ch02
import Ch02.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "doubleMe" $ do
    it "should double a number" $ do doubleMe 2 `shouldBe` 4
  describe "doubleUs" $ do
    it "should double two numbers and add them together" $ do
      doubleUs 4 9 `shouldBe` 26
  describe "doubleSmallNumber" $ do
    it "should double a number only if it is smaller or equal than 80" $ do
      doubleSmallNumber 80 `shouldBe` 160
