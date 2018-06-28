module Euler009 where

import Data.Maybe (listToMaybe)

import Euler039 (integralRightTriangles, perimeter, Triangle(..))

-- | The desired triangle perimeter.
target :: Int
target = 1000

-- | Produces all integral right triangles with the given perimeter.
trianglesWithPerimeter :: Int -> [Triangle]
trianglesWithPerimeter p = filter (\t -> perimeter t == p) . integralRightTriangles $ p

-- | Computes the product of the sides of a triangle.
sideProduct :: Triangle -> Int
sideProduct (Triangle a b c) =  a * b * c

-- | Produces the product of the sides of the triagnle with the given perimeter,
-- if such a triangle exists.
solution :: Int -> Maybe Int
solution = fmap sideProduct . listToMaybe . trianglesWithPerimeter

main :: IO ()
main = print $ solution target
