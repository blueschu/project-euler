module Euler132 where

import Data.List (iterate')

import Euler003 (primeFactors)

target :: Int
target = 10 ^ 9

repunit :: Int -> Integer
repunit k = head . drop k . iterate' ((+1) . (*10)) $ 0

main :: IO ()
main = print $ sum . take 40 . primeFactors . repunit $ target



