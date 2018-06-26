module Euler095 where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)

import Euler021 (properDivisorSum)

-- The value that no chain element may exceed
elementCap :: Int
elementCap = 999999

amicableChain :: Integral a => a -> Maybe [a]
amicableChain start = go [start]
  where
    go acc
      | next == start   = Just acc
      | next `elem` acc = Nothing
      | otherwise       = go (next:acc)
      where next = properDivisorSum $ head acc

noneExceeds :: Integral a => a -> [a] -> Bool
noneExceeds n = all (<=n)

solution :: Integral a => a -> a
solution cap = minimum .
    maximumBy (comparing length) .
    filter (noneExceeds cap) .
    mapMaybe amicableChain $ [1..cap]

main :: IO ()
main = print $ solution elementCap
