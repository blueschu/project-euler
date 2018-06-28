module Euler008 where

import Data.Char (isDigit, digitToInt)
import Data.Maybe (mapMaybe)
import Data.List (tails)

-- | The window size given in the problem.
windowSize :: Int
windowSize = 13

-- | Converts a character to the integer it represents if possible.
charToInt :: Char -> Maybe Int
charToInt c
  | isDigit c = Just $ digitToInt c
  | otherwise = Nothing

-- | Yields a sliding window of the given length over the given list.
--
-- The last n elements of the produced list will contain fewer than n elements,
-- where n is the window size.
--
-- @
-- windows 2 [1..4] == [[1,2],[2,3],[3,4],[4],[]]
-- @
windows :: Int -> [a] -> [[a]]
windows n = map (take n) . tails

-- | Computes the greatest product of a series of n adjacent elements in the
-- given list of integers.
solution :: Int -> [Int] -> Int
solution n = maximum .  map product . windows n

main :: IO ()
main = do
        input <- readFile "../resources/input008.txt"
        let digits = mapMaybe charToInt input
        print $ solution windowSize digits
