module Euler036 where

import Common (numericPalindrome)

target = 1000000

solution cap = sum . takeWhile (<cap) $
    [x | x <- [1..], numericPalindrome 10 x, numericPalindrome 2 x]

main = print $ solution target
