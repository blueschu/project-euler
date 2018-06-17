module Euler003 where

target = 600851475143

primeFactors :: Integral a => a -> [a]
primeFactors n = searchFactors 2 n
  where
    searchFactors _ 1 = []
    searchFactors _ 0 = [1..]
    searchFactors s n
      | n `mod` s == 0 = s:(searchFactors s (n `quot` s))
      | n < 0 = searchFactors s (-n)
      | otherwise = searchFactors (s + 1) n

greatestPrimeFactor :: Integral a => a -> a
greatestPrimeFactor = maximum . primeFactors

main = print $ greatestPrimeFactor target
