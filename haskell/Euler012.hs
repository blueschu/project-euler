module Euler012 where

import Common (factorPairs)

target = 500

triangleNumbers :: Integral a => [a]
triangleNumbers = scanl1 (+) [1..]

hasExcessFactors :: Integral a => a -> a -> Bool
hasExcessFactors min n = min < 2 * (fromIntegral . length . factorPairs) n

solution n = head [x | x <- triangleNumbers, hasExcessFactors n x]

main = print $ solution target 
