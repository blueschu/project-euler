module Euler034 where

import Common (digitFactorialSum)

target :: Int
target = 9999999 -- digit factorial sum of 2540160
                 -- greater values that satify the problem cannot exist

isOwnDigitFactorial :: Integral a => a -> Bool
isOwnDigitFactorial n = n == digitFactorialSum n

solution :: Integral a => a -> a
solution cap = sum $ filter isOwnDigitFactorial . takeWhile (<cap) $ [3..]

main :: IO ()
main = print $ solution target
