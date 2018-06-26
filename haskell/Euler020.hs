module Euler020 where

import Common (digitSum, factorial)

target :: Integer
target = 100

factorialDigitSum :: Integer -> Integer
factorialDigitSum = digitSum . factorial

main :: IO ()
main = print $ factorialDigitSum target
