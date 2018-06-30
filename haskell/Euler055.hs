module Euler055 where

import Common (numericPalindrome)

target :: Integer
target = 10000

checkDepth :: Int
checkDepth = 50

reverseNum :: Integral a => a -> a
reverseNum = fromIntegral . read . reverse . show . fromIntegral

-- | Checks whether an integer is a Lychrel number.
lychrel :: Integral a => Int -> a -> Bool
lychrel depth = all (not . numericPalindrome 10) . take depth . tail . iterate (\x -> x + reverseNum x)

-- | List of numbers considred to be lychrels when checked to the given depth.
lychrels :: Integral a => Int -> [a]
lychrels depth = filter (lychrel depth) [1..]

main :: IO ()
main = print . length . takeWhile (<target) $ lychrels checkDepth
