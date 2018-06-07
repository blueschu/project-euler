module Common (factorial, digitSum, fibonacci, factorPairs, factorSet) where

import qualified Data.Set as Set

factorial :: Integral a => a -> a
factorial n = product [2..n]

digitSum :: Integral a => a -> a
digitSum 0 = 0
digitSum n
    | n < 0 = digitSum (-n)
    | otherwise = end + digitSum rest
    where end = n `mod` 10
          rest = n `quot` 10

fibonacci :: Integral a => [a]
fibonacci = map fst $ iterate (\(a,b) -> (b, b + a)) (0, 1)

factorPairs :: Integral a => a -> [(a,a)]
factorPairs n = go n 1
    where top = 1 + (floor . sqrt . fromIntegral) n
          go n s
              | s >= top = []
              | n `mod` s == 0 = (s, div):(go n next)
              | otherwise = go n next
              where div = n `quot` s
                    next = succ s

factorSet :: Integral a => a -> Set.Set a
factorSet = Set.fromList . flattenPairs . factorPairs
     where flattenPairs = foldr (\(a, b) acc -> a : b : acc) []

