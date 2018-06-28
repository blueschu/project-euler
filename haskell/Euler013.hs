module Euler013 where

main :: IO ()
main = do
         input <- readFile "../resources/input013.txt"
         let numbers = map read . lines $ input
         print $ take 10 . show . sum $ (numbers :: [Integer])
