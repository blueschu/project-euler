module Euler027 where

import Data.List (foldl1')
import Data.Ord  (comparing)

import Common (isPrime)

-- | A quadratic relationship in the form of @n*n + a*n + b@.
quadratic :: Integral a => a -> a -> a -> a
quadratic a b n = n * n + a * n + b

-- | Computes the number of consecutive values, starting with 0, that may be
-- fed to the given formula to produce primes.
primeProductionLength :: (Int -> Int) -> Int
primeProductionLength f = length . takeWhile isPrime . map f $ [0..]

-- | Strict version of "Data.Foldable.maximumBy".
--
-- The default implementation of "maximumBy" operates in O(1) stack
-- space and is liable to overflow when applied to larges lists. This
-- implementation mitigates this risk by using a strict fold ("foldl1'").
--
-- While Refactoring the "solution" function removed the need for this
-- implementation, using a strict version of "maxmimumBy" still
-- significantly reduces the memory and computation time required when
-- running this solution.
maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' cmp = foldl1' max' where
    max' x y = case x `cmp` y of
                    GT -> x
                    _  -> y

-- | Searches for the coefficients of the quadratic relationship that
-- produces the most prime numbers when given consecutive values.
solution :: [Int] -- ^ Values for a.
         -> [Int] -- ^ Values for b.
         -> (Int ,Int) -- ^ The most effective coefficients.
solution as bs =
    let choices       = [(a, b) | a <- as, b <- bs]
        primeYield    = primeProductionLength . (uncurry quadratic)
        formulaYields = zip choices $ map primeYield choices
    in  fst $ maximumBy' (comparing snd) formulaYields



main :: IO ()
main =
    let domainA = [(-999)..999]
        domainB = [(-1000)..1000]
        (a, b)  = solution domainA domainB
    in  print $ a * b
