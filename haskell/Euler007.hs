module Euler007 where

import Common (primes)

-- | The zero-based index of the desired prime number
target :: Int
target = 10000

main :: IO ()
main = print $ primes !! target
