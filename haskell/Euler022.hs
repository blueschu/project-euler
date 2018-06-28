module Euler022 where

import Data.Char (ord)
import Data.List (sort)
import Data.List.Split (splitOn)

rawNameScore :: String -> Int
rawNameScore = let start = ord 'A'
                   pos c = 1 + ord c - start
               in  sum . map pos

nameScores :: [String] -> [Int]
nameScores names = let scores = map rawNameScore names
                   in  zipWith (*) scores [1..]

main :: IO ()
main = do
         input <- readFile "../resources/input022.txt"
         let names  = splitOn "," . filter (/='\"') $ input
             scores = nameScores . sort $ names
         print $ sum scores
