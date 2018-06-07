module Euler006 where

sumOfSquares :: Integral a => [a] -> a
sumOfSquares ns = sum . map (^2) $ ns

squareOfSum :: Integral a => [a] -> a
squareOfSum ns = (sum ns) ^ 2

solution ns = (squareOfSum ns) - (sumOfSquares ns)

main = print $ solution [1..100]
