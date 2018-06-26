module Euler029 where

import qualified Data.Set as Set

target :: Integer
target = 100

productDistinctPowers :: Integral a => [a] -> Set.Set a
productDistinctPowers ns = Set.fromList [x ^ y | x <- ns, y <- ns]

solution :: Integral a => [a] -> Int
solution = Set.size . productDistinctPowers

main :: IO ()
main = print $ solution [2..target]
