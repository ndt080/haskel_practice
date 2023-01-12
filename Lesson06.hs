module Lesson06 (testLesson06) where

import Data.List (unfoldr)

-- 01: Expand the natural number n into a list of all numbers smaller than it.
unfoldNatural = unfoldr (\a -> if a == 0 then Nothing else Just (a, a-1))

-- 02: Expand a number into a list of digits of its binary representation           
unfoldToBinFn n = 
  if n == 0 
  then Nothing 
  else let (a, b) = n `divMod` 2 in Just (b, a)

unfoldToBin n = reverse $ unfoldr unfoldToBinFn n

-- 03: Convert the list of digits by convolution to the value of a number
foldBin = foldr (\x y -> x + 2 * y) 0 . reverse



-- bintodec :: Integral i => i -> i
-- bintodec 0 = 0
-- bintodec i = 2 * bintodec (div i 10) + (mod i 10)

-- TESTING MODULE
testLesson06 :: IO ()
testLesson06 = do
  print ("------: LESSON 06 :----------")
  print ("01: unfoldNatural            (15):   ", unfoldNatural 15)
  print ("02: unfoldToBin              (26):   ", unfoldToBin 26)
  print ("03: foldBin         ([1,1,0,1,0]):   ", foldBin [1,1,0,1,0])
  -- print ("03: foldBin         ([1,1,0,1,0]):   ", bintodec 11010)
  print ("-------------------------------")