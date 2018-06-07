module Common (digitSum, fibonacci) where

digitSum :: Integral a => a -> a
digitSum 0 = 0
digitSum n
    | n < 0 = digitSum (-n)
    | otherwise = end + digitSum rest
    where end = n `mod` 10
          rest = n `quot` 10

fibonacci :: Integral a => [a]
fibonacci = map fst $ iterate (\(a,b) -> (b, b + a)) (0, 1)

