module Euler036 where

import Common (numericPalindrome)

target :: Int
target = 1000000

solution :: Integral a => a -> a
solution cap = sum . takeWhile (<cap) $
    [x | x <- [1..], numericPalindrome 10 x, numericPalindrome 2 x]

main :: IO ()
main = print $ solution target
