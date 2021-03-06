module Euler002 where

import Common (fibonacci)

target :: Int
target = 4000000

evenFibonacci :: Integral a => [a]
evenFibonacci = filter even fibonacci

solution :: Integral a => a -> a
solution cap = sum $ takeWhile (<cap) evenFibonacci

main :: IO ()
main = print $ solution target

-- Or, as a one-liner
-- main = print $ sum $ takeWhile (<4000000) [x | x <- map fst $ iterate (\(a,b) -> (b, b + a)) (0, 1), even x]
