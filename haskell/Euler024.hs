module Euler024 where

import Data.List (permutations, sort)

-- | Sample space of digits to permute.
sample :: [Char]
sample = "0123456789"

-- | Zero-based index of the desired permutation.
target :: Int
target = 999999

main :: IO ()
main = print . (!! target) . sort . permutations $ sample
