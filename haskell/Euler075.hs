module Euler075 where

import Data.List (group, sort)

import Euler039 (integralRightTriangles, perimeter, Triangle)

target :: Int
target = 1500000

uniquePerimeters :: [Triangle] -> [Int]
uniquePerimeters =  map head . filter ((==1) . length) . group . sort . map perimeter

main :: IO ()
main = print . length . uniquePerimeters . integralRightTriangles $ target
