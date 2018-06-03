target = 4000000

fibonacci :: Integral a => [a]
fibonacci = map fst $ iterate (\(a,b) -> (b, b + a)) (0, 1)

evenFibonacci :: Integral a => [a]
evenFibonacci = filter even fibonacci

solution :: Integral a => a -> a
solution cap = sum $ takeWhile (\x -> x < cap) evenFibonacci

main = print $ solution target

-- Or, as a one-liner
-- main = print $ sum $ takeWhile (\x -> x < 4000000) [x | x <- map fst $ iterate (\(a,b) -> (b, b + a)) (0, 1), even x]
