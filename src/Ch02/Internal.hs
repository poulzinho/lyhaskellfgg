module Ch02.Internal
  ( doubleMe
  , doubleUs
  , doubleSmallNumber
  ) where

doubleMe :: Integer -> Integer
doubleMe x = x + x

doubleUs :: Integer -> Integer -> Integer
-- doubleUs x y = x*2 + y*2
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x =
  if x > 80
    then x
    else x * 2
