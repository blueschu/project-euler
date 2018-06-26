module Euler028 where

spiralWidth :: Int
spiralWidth = 1001

spiralRadius :: Int
spiralRadius = (spiralWidth + 1) `quot` 2

-- See python solution for the derivation of this formula
spiralDiagonalSum :: Integral a => a -> a
spiralDiagonalSum 1 = 1
spiralDiagonalSum radius 
  | radius < 1 = error "Spiral radius cannot be less than 1"
  | otherwise = let ringSum = 16 * radius^2 - 28 * radius + 16
                in  ringSum + spiralDiagonalSum (pred radius)

main :: IO ()
main = print $ spiralDiagonalSum spiralRadius
