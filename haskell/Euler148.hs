module Euler148 where

import Common (nCr)
import Data.List (genericLength, intercalate)

target = 10 ^ 9

-- pascalRow' :: Integral a => a -> [a]
-- pascalRow' n = map (n `nCr`) [0..n]

pascalRow :: Integral a => a -> [a]
pascalRow n = left ++ right
  where
    halfn = n `quot` 2
    left = map (n `nCr`) [0..halfn]
    right = let trim = fromIntegral (1 - n `mod` 2)
            in drop trim . reverse $ left

solution n = sum . map (genericLength . filter p . pascalRow) $ [0..n - 1] where
    p x = x `mod` 7 /= 0

main = print $ solution target
