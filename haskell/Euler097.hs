module Euler097 where

nonMersennePrime = 28433 * (2^7830457) + 1

digitCount = 10

main = print $ nonMersennePrime `mod` (10 ^ digitCount)
