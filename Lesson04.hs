module Lesson04 (eGcd, pow, testLesson04) where

-- GCD
eGcd :: Int -> Int -> Int
eGcd a b
  | a <= 0 = 0
  | b <= 0 = 0
  | a > b = eGcd b (a - b)
  | a < b = eGcd (b - a) a
  | otherwise = a

-- POW
pow :: (Integral x, Num p) => p -> x -> p
pow x p
  | p == 0 = 1
  | p == 1 = x
  | even p = pow (x * x) (div p 2)
  | otherwise = x * pow x (p - 1)

-- POW with tail recursion
tailPowFn :: (Integral x, Num p) => p -> x -> p -> p
tailPowFn x p acc
  | p == 0 = acc
  | even p = tailPowFn (x * x) (div p 2) acc
  | otherwise = tailPowFn x (p - 1) (x * acc)

tailPow :: (Integral x, Num p) => p -> x -> p
tailPow x p = tailPowFn x p 1


-- TESTING MODULE
testLesson04 :: IO ()
testLesson04 = do
  print ("------: LESSON 04 :------")
  print ("eGcd(3, 27):    ", eGcd 3 27)
  print ("pow(3, 4):      ", pow 3 4)
  print ("tailPow(3, 4):  ", tailPow 3 4)
  print ("---------------------------")