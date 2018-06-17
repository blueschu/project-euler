module Euler021 where

import Common (factorSet)

target = 10000

properDivisorSum :: Integral a => a -> a
properDivisorSum n = (sum . factorSet $ n) - n

amicable :: Integral a => a -> Bool
amicable n = divisorSum /= n && properDivisorSum divisorSum  == n
  where divisorSum = properDivisorSum n

amicableSumBelow cap = sum . takeWhile (<cap) . filter amicable $ [1..]

main = print $ amicableSumBelow target
