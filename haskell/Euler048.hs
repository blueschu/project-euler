module Euler048 where

import Common (digitListBase, takeLast)

target :: Int
target = 1000

selfPowers :: Integral a => [a]
selfPowers = map (\x -> x ^ x) [1..]

solution :: Integral a => Int -> [a]
solution cap = let sumOfPowersBelow = sum . take cap $ selfPowers
                   digits = digitListBase 10 sumOfPowersBelow
               in  takeLast 10 digits

main :: IO ()
main = print $ solution target
