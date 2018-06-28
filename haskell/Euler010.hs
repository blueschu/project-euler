module Euler010 where

import Common (primes)

-- | Search cap for prime sumation
target :: Int
target = 2000000

-- | Computes the sum of all primes below the given integer.
sumOfPrimesBelow :: Int -> Int
sumOfPrimesBelow cap = sum . takeWhile (<cap) $ primes

main :: IO ()
main = print $ sumOfPrimesBelow target
