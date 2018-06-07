module Euler001 where

target = 1000

divisibleByAny :: Integral a => [a] -> a -> Bool
divisibleByAny ns num = any (\x -> num `mod` x == 0) ns

multiplesOfAny :: Integral a => [a] -> [a]
multiplesOfAny ns = [x | x <- [1..], divisibleByAny ns x]

solution cap = sum $ takeWhile (<cap) $ multiplesOfAny [3, 5]

main = print $ solution target

-- Or, as a one-liner
-- main = print $ sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]
    
