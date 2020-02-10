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
spec
-- Baby's first functions
 = do
  describe "doubleMe" $ do
    it "should double a number" $ do doubleMe 2 `shouldBe` 4
  describe "doubleUs" $ do
    it "should double two numbers and add them together" $ do
      doubleUs 4 9 `shouldBe` 26
  describe "doubleSmallNumber" $ do
    it "should double a number only if it is smaller or equal than 80" $ do
      doubleSmallNumber 80 `shouldBe` 160
-- An intro to lists
  describe "putting two lists together" $ do
    it "should put two list together by using ++" $ do
      [1, 2] ++ [3, 4] `shouldBe` [1, 2, 3, 4]
    it "should concatenate strings by using ++" $ do
      "hello " ++ "world!" `shouldBe` "hello world!"
    it "should concatenate arrays of chars by using ++" $ do
      ['h', 'a'] ++ ['l', 'l', 'o'] `shouldBe` "hallo"
    it "should put an item to the beginning of a list using the : operator" $ do
      'A' : " SMALL DOG" `shouldBe` "A SMALL DOG"
    it "should put a number to the beginning of numeric list using :" $ do
      5 : [4, 3, 2, 1] `shouldBe` [5, 4, 3, 2, 1]
    it "should allow to use list's sintactic sugar instead of the : operator" $ do
      [1, 2, 3] `shouldBe` 1 : 2 : 3 : []
    it "should allow to get an element of a list by the index with !! operator" $ do
      "Roger Rabbit" !! 6 `shouldBe` 'R'
    it "should compare list contents in lexicographical order" $ do
      [3, 4, 2] > [3, 5] `shouldBe` False
    it "should return the head of a list" $ do
      head [100, 4, 3, 2] `shouldBe` 100
    it "should return the tail of a list" $ do
      tail [100, 4, 3, 2, 1] `shouldBe` [4, 3, 2, 1]
    it "should return the last element of a list" $ do
      last [5, 4, 2, 1] `shouldBe` 1
    it "should return all elements but the last one of a list" $ do
      init [5, 4, 2, 1, 0] `shouldBe` [5, 4, 2, 1]
    it "should return the length of a list" $ do
      length [1, 2, 3, 4, 5] `shouldBe` 5
    it "should check if a list is empty" $ do
      null [1, 2, 3] `shouldBe` False
      null [] `shouldBe` True
    it "should reverse a list" $ do reverse [1, 2, 3] `shouldBe` [3, 2, 1]
    it "should take some elements of a list" $ do
      take 3 [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3]
      take 1000 [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3, 4, 5]
      take 0 [1, 2, 3, 4, 5] `shouldBe` []
    it "should drop some elements of a list" $ do
      drop 3 [1, 2, 3, 4, 5] `shouldBe` [4, 5]
    it "should return the biggest element of a list" $ do
      maximum [1, 2, 100, 3] `shouldBe` 100
    it "should return the smallest element of a list" $ do
      minimum [1, 2, 100, 3] `shouldBe` 1
    it "should return the sum of the elements of a list" $ do
      sum [1, 2, 3] `shouldBe` 6
    it "should return the product of the elements of a list" $ do
      product [1, 2, 3] `shouldBe` 6
      product [1, 2, 3, 0] `shouldBe` 0
    it "should verify if an element belongs to a list, a.k.a. infix function" $ do
      2 `elem` [1, 2, 3, 5] `shouldBe` True
      100 `elem` [1, 3, 5] `shouldBe` False
