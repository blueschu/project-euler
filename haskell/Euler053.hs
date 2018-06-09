module Eueler053 where

import Common (factorial)

targetSpace = 100
targetCombinations = 1000000

combinationCount :: Integral a => a -> a -> a
combinationCount n r = (factorial n) `quot` (factorial (n - r) * factorial r)

solution spaceCap minCombinations = sum [1 | x <- [1..spaceCap], y <- [1..x], combinationCount x y > minCombinations]

main = print $ solution targetSpace targetCombinations
