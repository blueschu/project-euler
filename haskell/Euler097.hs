module Euler097 where

nonMersennePrime :: Integer
nonMersennePrime = 28433 * (2^7830457) + 1

digitCount :: Int
digitCount = 10

main :: IO ()
main = print $ nonMersennePrime `mod` (10 ^ digitCount)
