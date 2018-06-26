module Euler005 where

target :: Int
target = 20

divisibleByAll :: Integral a => [a] -> a -> Bool
divisibleByAll ns num = all (\x -> num `mod` x == 0) ns

solution :: Integral a => a -> a
solution cap = head [x | x <- [1..], divisibleByAll [1..cap] x]

main :: IO ()
main = print $ solution target
