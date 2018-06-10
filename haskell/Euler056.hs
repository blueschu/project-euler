module Euler056 where

import Common (digitSum)

target = 100

productPowers :: Integral a => a -> [a]
productPowers cap = [a ^ b | a <- [1..cap - 1], b <- [1..cap - 1]]

greatestDigitSum :: Integral a => [a] -> a
greatestDigitSum = maximum . map digitSum

main = print . greatestDigitSum $ productPowers target
