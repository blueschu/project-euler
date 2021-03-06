module Euler025 where

import Data.List (findIndex)

import Common (fibonacci)

target :: Int
target = 1000

digitCount :: (Show a, Integral b) => a -> b
digitCount = fromIntegral . length . show

solution :: Integral a => a -> Maybe Int
solution digits = findIndex (\x -> digitCount x == digits) fibonacci

main :: IO ()
main = print $ solution target
