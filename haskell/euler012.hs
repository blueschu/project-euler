target = 500

triangleNumbers :: Integral a => [a]
triangleNumbers = go 1 2
    where go n i = n:(go (n + i) (succ i))

factorPairs :: Integral a => a -> [(a,a)]
factorPairs n = go n 1
    where top = 1 + (floor . sqrt . fromIntegral) n
          go n s
              | s >= top = []
              | n `mod` s == 0 = (s, div):(go n next) 
              | otherwise = go n next
              where div = n `quot` s
                    next = succ s

hasExcessFactors :: Integral a => a -> a -> Bool
hasExcessFactors min n = min < 2 * (fromIntegral . length . factorPairs) n

solution n = head [x | x <- triangleNumbers, hasExcessFactors n x]

main = print $ solution target 
