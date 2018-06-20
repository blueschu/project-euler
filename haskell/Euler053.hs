module Eueler053 where

import Common (nCr)

targetSpace = 100
targetCombinations = 1000000

solution spaceCap minCombinations = sum [1 | x <- [1..spaceCap], y <- [1..x], x `nCr` y > minCombinations]

main = print $ solution targetSpace targetCombinations
