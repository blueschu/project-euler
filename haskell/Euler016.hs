target = 2 ^ 1000

digitSum :: Integral a => a -> a
digitSum 0 = 0
digitSum n
    | n < 0 = digitSum (-n)
    | otherwise = end + digitSum rest
    where end = n `mod` 10
          rest = n `quot` 10

main = print $ digitSum target
