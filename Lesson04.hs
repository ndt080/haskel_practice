module Lesson04 (eGcd, pow, testLesson04) where

-- 01: GCD
eGcd :: Int -> Int -> Int
eGcd a b
  | a <= 0 = 0
  | b <= 0 = 0
  | a > b = eGcd b (a - b)
  | a < b = eGcd (b - a) a
  | otherwise = a

-- 02: POW
pow :: (Integral x, Num p) => p -> x -> p
pow x p
  | p == 0 = 1
  | p == 1 = x
  | even p = pow (x * x) (div p 2)
  | otherwise = x * pow x (p - 1)

-- 02: POW with tail recursion
tailPowFn :: (Integral x, Num p) => p -> x -> p -> p
tailPowFn x p acc
  | p == 0 = acc
  | even p = tailPowFn (x * x) (div p 2) acc
  | otherwise = tailPowFn x (p - 1) (x * acc)

tailPow :: (Integral x, Num p) => p -> x -> p
tailPow x p = tailPowFn x p 1

-- 04: Perfect numbers
perfect :: Integral a => a -> Bool
perfect n = n == sum [i | i <- [1..n-1], n `mod` i == 0]

-- 05: The Syracuse Sequence 
collatzFn :: Int -> Int -> Int
collatzFn x acc
  | x == 1 = acc
  | even x = collatzFn (div x 2) (acc + 1)
  | otherwise = collatzFn (3 * x + 1) (acc + 1)

collatz :: Int -> Int
collatz x = collatzFn x 1

-- 06: Delannois numbers
delannoy :: Int -> Int -> Int
delannoy n m 
  | n == 0 = 1
  | m == 0 = 1
  | otherwise = 
    let
      n' = n - 1
      m' = m - 1
    in delannoy n' m + delannoy n' m' + delannoy n m'

-- TESTING MODULE
testLesson04 :: IO ()
testLesson04 = do
  print ("------: LESSON 04 :----------")
  print ("01: eGcd    (3, 27):     ", eGcd 3 27)
  print ("02: pow      (3, 4):     ", pow 3 4)
  print ("02: tailPow  (3, 4):     ", tailPow 3 4)
  print ("04: perfect    (28):     ", perfect 28)
  print ("05: collatz     (7):     ", collatz 7)
  print ("06: delannoy (3, 4):     ", delannoy 3 4)
  print ("-------------------------------")