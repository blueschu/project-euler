module Euler095 where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)

import Euler021 (properDivisorSum)

-- | The value that no chain element may exceed.
elementCap :: Int
elementCap = 999999

-- | Produces the amicable chain that begins with the given value, if
-- one exists.
--
-- If a possible amicable chain would contain a value greater than
-- "cap", it will be ignored.
amicableChain :: Integral a
              => a -- ^ The value that no chain element may exceed.
              -> a -- ^ The initial value for the chain.
              -> Maybe [a] -- ^ The amicable chain that begins with the given seed
amicableChain cap start = go [start]
  where
    go acc
      | next == start                  = Just acc
      | next `elem` acc || next > cap  = Nothing
      | otherwise                      = go (next:acc)
      where next = properDivisorSum $ head acc

-- Finds the smallest memebrs of the longest amicable chain with no
-- element exceeding the given cap.
solution :: Integral a => a -> a
solution cap = minimum .
    maximumBy (comparing length) .
    mapMaybe (amicableChain cap) $ [1..cap]

main :: IO ()
main = print $ solution elementCap
