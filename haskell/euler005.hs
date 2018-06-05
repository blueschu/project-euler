target = 20

divisibleByAll :: Integral a => [a] -> a -> Bool
divisibleByAll ns num = all (\x -> num `mod` x == 0) ns

solution cap = head $ [x | x <- [1..], divisibleByAll [1..cap] x]

main = print $ solution target
