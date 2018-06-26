module Euler014 where

import Data.List (maximumBy)
import Data.Ord (comparing)

target :: Int
target = 1000000

collatzLength :: Integral a => a -> a
collatzLength 1 = 1
collatzLength n = 1 + collatzLength next
  where next = if even n then n `quot` 2 else 3 * n + 1

solution :: Integral a => a -> a
solution cap = maximumBy (comparing collatzLength) [2..cap]

main :: IO ()
main = print $ solution target
