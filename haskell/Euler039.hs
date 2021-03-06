module Euler039 where

import Data.List (group, maximumBy, sort)
import Data.Ord (comparing)

import Common (isInteger)

target :: Int
target = 1000

data Triangle = Triangle Int Int Int deriving (Show)

perimeter :: Triangle -> Int
perimeter (Triangle a b c) = a + b + c

integralRightTriangles :: Int -> [Triangle]
integralRightTriangles perimeterCap = [t | -- todo: find a more clean way of expressing this - perhaps with 'do'?
    a <- [3..perimeterCap `quot` 2],
    b <- [a..perimeterCap `quot`  2],
    let hypot = sqrt . fromIntegral $ a ^ 2 + b ^ 2,
    isInteger 15 hypot,
    let t = Triangle a b (floor  hypot),
    perimeter t <= perimeterCap]

mostFrequentRightTrianglePerimeter :: Int -> (Int, Int)
mostFrequentRightTrianglePerimeter cap = maximumBy (comparing snd)
    . map (\x -> (head x, length x))
    . group
    . sort
    . map perimeter $ integralRightTriangles cap

main :: IO ()
main = print $ mostFrequentRightTrianglePerimeter target

