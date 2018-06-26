module Euler055 where

import Common (numericPalindrome)

target :: Integer
target = 10000

checkDepth :: Int
checkDepth = 50

reverseNum :: Integral a => a -> a
reverseNum = fromIntegral . read . reverse . show . fromIntegral

lychrel :: Integral a => Int -> a -> Bool
lychrel depth = any (numericPalindrome 10) . take depth . drop 1 . iterate (\x -> x + reverseNum x)

lychrels :: Integral a => Int -> [a]
lychrels depth = filter (lychrel depth) [1..]

main :: IO ()
main = print . length . takeWhile (<target) $ lychrels checkDepth
