module Euler034 where

-- Factorial function replaced with local patterning matching to reduce computation time.
-- import Common (factorial)
import Common (digitListBase)

target :: Int
target = 9999999 -- digit factorial sum of 2540160
                 -- greater values that satify the problem cannot exist

isOwnDigitFactorial :: Integral a => a -> Bool
isOwnDigitFactorial n = n == (sum . map digitFactorial . digitListBase 10 $ n)
    where digitFactorial :: Integral a => a -> a
          digitFactorial 0 = 1
          digitFactorial 1 = 1
          digitFactorial 2 = 2
          digitFactorial 3 = 6
          digitFactorial 4 = 24
          digitFactorial 5 = 120
          digitFactorial 6 = 720
          digitFactorial 7 = 5040
          digitFactorial 8 = 40320
          digitFactorial 9 = 362880

solution :: Integral a => a -> a
solution cap = sum $ filter isOwnDigitFactorial . takeWhile (<cap) $ [3..]

main = print $ solution target
