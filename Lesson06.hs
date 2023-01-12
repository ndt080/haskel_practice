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

--04: Expand a number into a list of its prime divisors.
unfoldToPrimeDivsFn n =
  let list = [(f, div n f) | f <- [2..n], mod n f == 0] in
  if list == [] 
  then Nothing 
  else Just (head list)

unfoldToPrimeDivs = unfoldr unfoldToPrimeDivsFn

--05: Express the list of the first n Fibonacci numbers through a sweep.
foldToFibonacciFn (prev, curr) = Just (curr, (curr, prev + curr))

foldToFibonacci n = take n $ unfoldr foldToFibonacciFn (0, 1)

--06: 


-- TESTING MODULE
testLesson06 :: IO ()
testLesson06 = do
  print ("------: LESSON 06 :----------")
  print ("01: unfoldNatural            (15):   ", unfoldNatural 15)
  print ("02: unfoldToBin              (26):   ", unfoldToBin 26)
  print ("03: foldBin         ([1,1,0,1,0]):   ", foldBin [1,1,0,1,0])
  print ("04: unfoldToPrimeDivs        (70):   ", unfoldToPrimeDivs 70)
  print ("05: foldToFibonacci          (70):   ", foldToFibonacci 4)
  print ("-------------------------------")