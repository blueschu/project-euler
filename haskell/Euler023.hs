module Euler023 where

import qualified Data.Set as Set

import Common (factorSet)

-- | Search cap given in problem.
--
-- All integers greater than this value can be written as the sum of
-- two abundant numbers.
knownCap :: Int
knownCap = 28123

-- | Checks whether a given integer is considered abundant.
abundant :: Integral a => a -> Bool
abundant n =
    let divisorSum = (sum . factorSet $ n) - n
    in  divisorSum > n

-- | An infinite list of every abundant number.
abundants :: [Int]
abundants = filter abundant [1..]

-- | The set of all numbers that cannot be written as the sum of two
-- abundant numbers.
solution :: Int -> Set.Set Int
solution searchCap =
    let abundantsBelowCap  = takeWhile (<=searchCap) abundants
        sumsOfTwoAbundants = map sum . sequence . replicate 2 $ abundantsBelowCap
        invalids           = Set.fromList sumsOfTwoAbundants
        everything         = Set.fromList [1..searchCap]
    in  everything `Set.difference` invalids

main :: IO ()
main = print . sum $ solution knownCap
