module Euler132 where

import Data.List (iterate')

import Euler003 (primeFactors)

target = 10 ^ 9

repunit :: Integral a => Int -> a
repunit k = head . drop k . iterate' ((+1) . (*10)) $ 0

main = print $ sum . take 40 . primeFactors . repunit $ target



