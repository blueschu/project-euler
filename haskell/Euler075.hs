module Euler075 where

import Data.List (group, sort)

import Euler039 (integralRightTriangles, perimeter, Triangle)

target = 1500000

uniquePerimeters :: [Triangle] -> [Int]
uniquePerimeters =  map head . filter ((==1) . length) . group . sort . map perimeter

main = print . length . uniquePerimeters . integralRightTriangles $ target
