module Euler053 where

import Common (nCr)

targetSpace, targetCombinations :: Integer
targetSpace = 100
targetCombinations = 1000000

solution :: Integral a => a -> a -> a
solution spaceCap minCombinations = sum [1 | x <- [1..spaceCap], y <- [1..x], x `nCr` y > minCombinations]

main :: IO ()
main = print $ solution targetSpace targetCombinations
