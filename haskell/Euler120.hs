module Euler120 where

problemSquareRemainder :: Integral a => a -> a -> a
problemSquareRemainder a n = numerator `mod` square
  where
    numerator = (a - 1) ^ n + (a + 1) ^ n
    square = a ^ 2

maxSquareRemainder :: Integral a => a -> a
maxSquareRemainder a = maximum . map (problemSquareRemainder a) $ [1..a ^ 2 - 1]

main :: IO ()
main = print . sum . map maxSquareRemainder $ [3..1000]
