module Euler095 where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Maybe (catMaybes)

import Euler021 (properDivisorSum)

-- The value that no chain element may exceed
elementCap = 999999

amicableChain :: Integral a => a -> Maybe [a]
amicableChain n = go n [n]
  where
    start = n
    go n acc
      | next == start = Just acc
      | next `elem` acc = Nothing
      | otherwise = go next (next:acc)
      where next = properDivisorSum n

noneExceeds :: Integral a => a -> [a] -> Bool
noneExceeds n = all (<=n)

solution :: Integral a => a -> a
solution cap = minimum .
    maximumBy (comparing length) .
    filter (noneExceeds cap) .
    catMaybes .
    map amicableChain $ [1..cap]

main = print $ solution elementCap
