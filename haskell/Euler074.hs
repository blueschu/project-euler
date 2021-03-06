module Euler074 where

import qualified Data.Set as Set

import Euler034 (digitFactorialSum)

searchCap :: Int
searchCap = 999999

targetLength :: Int
targetLength = 60

takeWhileUnique :: (Ord a) => [a] -> [a]
takeWhileUnique = go Set.empty
  where
    go :: (Ord a) => Set.Set a -> [a] -> [a]
    go _ [] = []
    go prior (x:xs)
      | x `Set.member` prior = []
      | otherwise = x:go (Set.insert x prior) xs

digitFactorialChain :: Integral a => a -> [a]
digitFactorialChain = iterate digitFactorialSum

nonRepeatingFactorialChainLength :: (Integral a) => a -> Int
nonRepeatingFactorialChainLength = length . takeWhileUnique . digitFactorialChain

-- Produces 402
main :: IO ()
main = print . length . filter ((==targetLength) . nonRepeatingFactorialChainLength) $ [1..searchCap]
