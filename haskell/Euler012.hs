module Euler012 where

import Common (factorPairs)

target :: Int
target = 500

triangleNumbers :: Integral a => [a]
triangleNumbers = scanl1 (+) [1..]

hasExcessFactors :: Integral a => a -> a -> Bool
hasExcessFactors min n = min < 2 * (fromIntegral . length . factorPairs) n

solution :: Integral a => a -> a
solution n = head [x | x <- triangleNumbers, hasExcessFactors n x]

main :: IO ()
main = print $ solution target 
