module Euler034 where

import Common (digitListBase, factorial)

target :: Int
target = 9999999 -- digit factorial sum of 2540160
                 -- greater values that satify the problem cannot exist

-- | Cache of the factorials of the digits 0-9.
factorials :: [Int]
factorials = map factorial [0..9]

-- | Computes the sum of the factorials of the decimal digits of an integer.
digitFactorialSum :: Integral a => a -> a
digitFactorialSum = fromIntegral . sum . map digitFactorial . digitListBase 10 where
    digitFactorial n = factorials !! (fromIntegral n)

isOwnDigitFactorial :: Integral a => a -> Bool
isOwnDigitFactorial n = n == digitFactorialSum n

solution :: Integral a => a -> a
solution cap =  sum . filter isOwnDigitFactorial $ [3..cap]

main :: IO ()
main = print $ solution target
