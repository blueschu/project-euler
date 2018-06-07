module Euler020 where

import Common (digitSum, factorial)

target = 100

factorialDigitSum = digitSum . factorial

main = print $ factorialDigitSum target
