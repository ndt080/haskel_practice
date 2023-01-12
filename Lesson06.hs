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


-- TESTING MODULE
testLesson06 :: IO ()
testLesson06 = do
  print ("------: LESSON 06 :----------")
  print ("01: unfoldNatural            (15):   ", unfoldNatural 15)
  print ("02: unfoldToBin              (26):   ", unfoldToBin 26)
  print ("-------------------------------")