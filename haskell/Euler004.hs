module Euler004 where

import Common (numericPalindrome)

greatestPalindrome :: Integral a => [a] -> a
greatestPalindrome = (maximum . filter (numericPalindrome 10))

productsOfNDigitNumbers :: Integral a => a -> [a]
productsOfNDigitNumbers digits = [a * b | a <- [bottom..top], b <- [a..top]]
    where bottom = 10 ^ (digits - 1)
          top = 10 ^ digits - 1

main = print $ greatestPalindrome $ productsOfNDigitNumbers 3
